;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-string (&key (quasi-quote-character #\[)
                                         (quasi-quote-end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body) (chain-transform transform (make-string-quasi-quote body)))
   (lambda (form spliced) (make-string-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-string-to-string ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(string)))

(define-syntax quasi-quoted-string-to-string-emitting-form ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(string-emitting-form)))

(define-syntax quasi-quoted-string-to-binary ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(quasi-quoted-binary binary)))

(define-syntax quasi-quoted-string-to-binary-emitting-form ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(quasi-quoted-binary binary-emitting-form)))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted string is made of string, list, string-quasi-quote, string-unquote nodes recursively.

(def ast string)

(def class* string-syntax-node (syntax-node)
  ())

(def (class* e) string-quote (quote string-syntax-node)
  ())

(def (function e) make-string-quote (body)
  (make-instance 'string-quote :body body))

(def (class* e) string-quasi-quote (quasi-quote string-syntax-node)
  ())

(def (function e) make-string-quasi-quote (body)
  (make-instance 'string-quasi-quote :body body))

(def (class* e) string-unquote (unquote string-syntax-node)
  ())

(def (function e) make-string-unquote (form &optional (spliced? #f))
  (make-instance 'string-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def special-variable *string-stream*)

(def function reduce-subsequences (sequence predicate reducer)
  (iter (with completely-reduced? = #t)
        (with length = (length sequence))
        (for index :from 0 :below length)
        (for reducibles = (iter (while (< index length))
                                (for element = (elt sequence index))
                                (while (funcall predicate element))
                                (collect element)
                                (incf index)))
        (collect (if (zerop (length reducibles))
                     (progn
                       (setf completely-reduced? #f)
                       (elt sequence index))
                     (progn
                       (decf index)
                       (apply reducer reducibles)))
          :into result)
        (finally (return (values result completely-reduced?)))))

(def function write-quasi-quoted-string (node)
  (etypecase node
    (string (write-string node *string-stream*))
    (list (mapc #'write-quasi-quoted-string node))
    (string-quote (write-quasi-quoted-string (body-of node))))
  (values))

(def macro with-quasi-quoted-string-emitting-environment (&body forms)
  `(bind ((*string-stream* (make-string-output-stream)))
     ,@forms
     (get-output-stream-string *string-stream*)))

(def (macro e) force-quasi-quoted-string (node)
  `(with-quasi-quoted-string-emitting-environment
     (write-quasi-quoted-string ,node)))

(def function transform-quasi-quoted-string-to-string-emitting-form (input &key (toplevel #t) (stream '*string-stream*) &allow-other-keys)
  (etypecase input
    (string-quasi-quote
     (labels ((process (node)
                (etypecase node
                  (string
                   (if (= 1 (length node))
                       `(write-char ,(char node 0) ,stream)
                       `(write-string ,node ,stream)))
                  (string-unquote `(write-quasi-quoted-string ,(transform 'string-emitting-form node :toplevel #f)))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((forms (reduce-subsequences (flatten (body-of input))
                                          #'stringp
                                          (lambda (&rest elements)
                                            (apply #'concatenate 'string elements))))
              (processed-forms (if (and toplevel
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms))))
         (if (and toplevel
                  (not (single-string-list-p processed-forms))
                  (eq stream '*string-stream*))
             `(with-quasi-quoted-string-emitting-environment
                ,@processed-forms)
             `(progn
                ,@processed-forms)))))
    (string-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'string-quasi-quote)
                     (transform 'string-emitting-lambda-form form :toplevel #f)
                     form))))))

(def method transform ((to (eql 'string-emitting-form)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-string-emitting-form input args))

(def function transform-quasi-quoted-string-to-quasi-quoted-binary (node &key (encoding :utf-8) &allow-other-keys)
  (etypecase node
    (list (mapcar #'transform-quasi-quoted-string-to-quasi-quoted-binary node))
    (string (babel:string-to-octets node :encoding encoding))
    (string-quasi-quote
     (make-instance 'binary-quasi-quote :body (transform-quasi-quoted-string-to-quasi-quoted-binary (body-of node))))
    (string-unquote
     (make-instance 'binary-unquote
                    :form `(transform-quasi-quoted-string-to-quasi-quoted-binary
                            ,(map-filtered-tree (form-of node) 'string-quasi-quote #'transform-quasi-quoted-string-to-quasi-quoted-binary))))
    (quote node)
    (quasi-quote node)
    (unquote node)))

(def method transform ((to (eql 'quasi-quoted-binary)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary input args))
