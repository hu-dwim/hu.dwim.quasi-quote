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

(def function wrap-form-using (form operator level)
  (iter (repeat level)
        (setf form (list operator form)))
  form)

(def function transform-quasi-quoted-list-to-list-emitting-form (input)
  (transformation-typecase input
    (list-quasi-quote
     (bind ((*quasi-quoted-list-nesting-level* 0))
       (transform-quasi-quoted-list-to-list-emitting-form/quasi-quote input)))))

(def function bq (&rest args)
  ;; TODO only for readability while testing
  (apply #'list args))

(def function transform-quasi-quoted-list-to-list-emitting-form/quasi-quote (input)
  (assert (typep input 'list-quasi-quote))
  (labels ((recurse (node)
             (typecase node
               (list-unquote (bind ((*quasi-quoted-list-nesting-level* (1- *quasi-quoted-list-nesting-level*)))
                               (recurse (form-of node))))
               (list-quasi-quote (if (compatible-transformation-pipelines?
                                      (transformation-pipeline-of input)
                                      (transformation-pipeline-of node))
                                     (bind ((*quasi-quoted-list-nesting-level* (1+ *quasi-quoted-list-nesting-level*)))
                                       (recurse (body-of node)))
                                     (transform node)))
               (list (list* (wrap-form-using 'bq 'quote (1- *quasi-quoted-list-nesting-level*))
                            (mapcar #'recurse node)))
               (t (wrap-form-using node 'quote *quasi-quoted-list-nesting-level*)))))
    (recurse input)))
