;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(define-syntax quasi-quoted-list (&key (start-character #\`)
                                       end-character
                                       (unquote-character #\,)
                                       (splice-character #\@)
                                       transformation-pipeline
                                       dispatched-quasi-quote-name)
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (bind ((toplevel? (= 1 *quasi-quote-nesting-level*)))
       (if toplevel?
           `(list-quasi-quote/toplevel ,body ,transformation-pipeline)
           (make-list-quasi-quote transformation-pipeline body))))
   (lambda (form spliced)
     (make-list-unquote form spliced))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :dispatched-quasi-quote-name dispatched-quasi-quote-name))

(define-syntax quasi-quoted-list-to-list-emitting-form (&key (with-inline-emitting :as-is)
                                                             (start-character #\`)
                                                             end-character
                                                             (unquote-character #\,)
                                                             (splice-character #\@)
                                                             dispatched-quasi-quote-name)
  (set-quasi-quoted-list-syntax-in-readtable
   :transformation-pipeline (list (make-instance 'quasi-quoted-list-to-list-emitting-form
                                                 :with-inline-emitting with-inline-emitting))
   :dispatched-quasi-quote-name dispatched-quasi-quote-name
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

;; `(a b ,c)
;; (list (quote a) (quote b) c)

;; `(a `(b ,c ,,d))
;; (list (quote a) (list (quote (quote b)) (quote c) d))


(def reader-stub list-quasi-quote (body transformation-pipeline)
  (bind ((expanded-body (recursively-macroexpand-reader-stubs body -environment-))
         (quasi-quote-node (make-list-quasi-quote transformation-pipeline expanded-body)))
    (run-transformation-pipeline quasi-quote-node)))

(def reader-stub list-unquote (form spliced?)
  (make-list-unquote form spliced?))


;;;;;;;
;;; AST

(def ast list)

(def class* list-syntax-node (syntax-node)
  ())

(def (class* e) list-quasi-quote (quasi-quote list-syntax-node)
  ())

(def (function e) make-list-quasi-quote (transformation-pipeline body)
  (make-instance 'list-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) list-unquote (unquote list-syntax-node)
  ())

(def (function e) make-list-unquote (form &optional (spliced? #f))
  (make-instance 'list-unquote :form form :spliced spliced?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform to list emitting form

(def (transformation e) quasi-quoted-list-to-list-emitting-form (lisp-form-emitting-transformation)
  ()
  'transform-quasi-quoted-list-to-list-emitting-form
  (:default-initargs :with-inline-emitting :as-is))

(def method compatible-transformations? and ((a quasi-quoted-list-to-list-emitting-form)
                                             (b quasi-quoted-list-to-list-emitting-form))
  (and (eql (with-inline-emitting? a) (with-inline-emitting? b))
       (eql (stream-variable-name-of a) (stream-variable-name-of b))
       (equalp (declarations-of a) (declarations-of b))))

(def special-variable *quasi-quoted-list-nesting-level*)

(def function transform-quasi-quoted-list-to-list-emitting-form (input)
  (transformation-typecase input
    (list-quasi-quote
     (bind ((*quasi-quoted-list-nesting-level* 0))
       (transform-quasi-quoted-list-to-list-emitting-form/quasi-quote input)))))

(def function bql (&rest args)
  ;; TODO only for readability while testing
  (apply #'list args))

(def function bqnc (&rest args)
  ;; TODO only for readability while testing
  (apply #'nconc args))

;; `(,x)
;; (BQL X)

;; ``(,,x)
;; (BQL X)

;; ``(,x)
;; (BQL 'BQL 'X)

;; `(a (b c) ,(list 1 2))
;; (bql 'a '(b c) (list 1 2))

;; `(a (b) ,(list 1) `(b ,(list 2 ,3)) c)
;; (BQL 'A (BQL 'B) (LIST 1) (BQL 'BQL ''B '(LIST '2 3)) 'C)

;; `(a (b) ,(list 1) `(b ,@(list 2 ,3)) c)
;; (BQL 'A (BQL 'B) (LIST 1) (BQL 'BQNC '(list 'B) (BQL 'LIST '2 3)) 'C)

(def function transform-quasi-quoted-list-to-list-emitting-form/quasi-quote (input)
  (labels ((unquote (node)
             (bind ((*quasi-quoted-list-nesting-level* (1- *quasi-quoted-list-nesting-level*)))
               (assert (<= 0 *quasi-quoted-list-nesting-level*) ()
                       "Nesting error in list quasi quoting, too many unquotes")
               (process-unquoted-form (form-of node))))
           (process-unquoted-form (node)
             (typecase node
               (list-unquote (unquote node))
               (list (if (zerop *quasi-quoted-list-nesting-level*)
                         (mapcar #'recurse node)
                         `(bql ,@(mapcar #'recurse node))))
               (t node)))
           (lift-quoted-form (node)
             (bind ((original-nodes node)
                    (spliced-count 0)
                    (unquote-count 0)
                    (prcessed-nodes (iter (for node :in original-nodes)
                                          (when (typep node 'list-unquote)
                                            (incf unquote-count)
                                            (when (spliced-p node)
                                              (incf spliced-count)))
                                          (collect (recurse node)))))
               (cond ((zerop unquote-count)
                      `(bql ,@prcessed-nodes))
                     ((not (zerop spliced-count))
                      `(,(wrap-form-using 'bqnc 'quote (1- *quasi-quoted-list-nesting-level*))
                        ,@(mapcar (lambda (original-node processed-node)
                                    (if (and (typep original-node 'list-unquote)
                                             (spliced-p original-node))
                                        processed-node
                                        `(,@(iter (for idx :from 0 :below (1- *quasi-quoted-list-nesting-level*))
                                                  (collect (wrap-form-using 'bql 'quote idx)))
                                          ,processed-node)))
                                   original-nodes prcessed-nodes)))
                     (t
                      `(bql ,@prcessed-nodes)))))
           (recurse (node)
             (typecase node
               (list-unquote (unquote node))
               (list-quasi-quote (if (compatible-transformation-pipelines?
                                      (transformation-pipeline-of input)
                                      (transformation-pipeline-of node))
                                     (bind ((*quasi-quoted-list-nesting-level* (1+ *quasi-quoted-list-nesting-level*)))
                                       `(,@(iter (for idx :from 0 :below (1- *quasi-quoted-list-nesting-level*))
                                                 (collect (wrap-form-using 'bql 'quote idx)))
                                         ,@(lift-quoted-form (body-of node))))
                                     (progn
                                       (break "This should not happen! Probably your reader setup is incorrect... otherwise send a mail to the list!")
                                       (transform node))))
               (list (lift-quoted-form node))
               (t (wrap-form-using node 'quote *quasi-quoted-list-nesting-level*)))))
    (recurse input)))

(def function wrap-form-using (form operator level)
  (iter (repeat level)
        (setf form (list operator form)))
  form)


#|

TODO delme, kept for reference

(def function transform-quasi-quoted-list-to-list-emitting-form/quasi-quote (input)
  (labels ((recurse (node)
             (typecase node
               (list-unquote (bind ((*quasi-quoted-list-nesting-level* (1- *quasi-quoted-list-nesting-level*)))
                               (assert (<= 0 *quasi-quoted-list-nesting-level*) ()
                                       "Nesting error in list quasi quoting, too many unquotes")
                               (recurse (form-of node))))
               (list-quasi-quote (if (compatible-transformation-pipelines?
                                      (transformation-pipeline-of input)
                                      (transformation-pipeline-of node))
                                     (bind ((*quasi-quoted-list-nesting-level* (1+ *quasi-quoted-list-nesting-level*)))
                                       (recurse (body-of node)))
                                     (progn
                                       (break "This should not happen! Probably your reader setup is incorrect... otherwise send a mail to the list!")
                                       (transform node))))
               (list `(,@(iter (for idx :from 0 :below *quasi-quoted-list-nesting-level*)
                               (collect (wrap-form-using 'bql 'quote idx)))
                       ,@(mapcar #'recurse node)))
               (t (wrap-form-using node 'quote *quasi-quoted-list-nesting-level*)))))
    (recurse input)))

|#