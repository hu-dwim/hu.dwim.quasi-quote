;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(def special-variable *quasi-quoted-lisp-nesting-level*)

(define-syntax quasi-quoted-lisp (&key (quasi-quote-character #\`)
                                       (quasi-quote-end-character nil)
                                       (unquote-character #\,)
                                       (splice-character #\@)
                                       (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-lisp-quasi-quote body))))
   (lambda (form spliced)
     (make-lisp-unquote form spliced))
   '*quasi-quoted-lisp-nesting-level*
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-lisp-to-lisp-emitting-form ()
  (set-quasi-quoted-lisp-syntax-in-readtable :transform '(lisp-emitting-form)))

;;;;;;;
;;; AST

(def ast lisp)

(def class* lisp-syntax-node (syntax-node)
  ((name)))

(def (class* e) lisp-quasi-quote (quasi-quote lisp-syntax-node)
  ())

(def (function e) make-lisp-quasi-quote (body)
  (make-instance 'lisp-quasi-quote :body body))

(def (class* e) lisp-unquote (unquote lisp-syntax-node)
  ())

(def (function e) make-lisp-unquote (form &optional (spliced? #f))
  (make-instance 'lisp-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def function transform-quasi-quoted-lisp-to-lisp-emitting-form (input &key (toplevel #t))
  (etypecase input
    (lisp-quasi-quote
     (labels ((process (node)
                (etypecase node
                  (lisp-unquote (transform-quasi-quoted-lisp-to-lisp-emitting-form node :toplevel #f))
                  (list `(list ,@(mapcar #'process node)))
                  (t (list 'quote node)))))
       (bind ((form (process (body-of input))))
         (if toplevel
             `(make-lisp-quasi-quote
               ,form)
             form))))
    (lisp-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'lisp-quasi-quote)
                     (transform-quasi-quoted-lisp-to-lisp-emitting-form form :toplevel #f)
                     form))))))

(def method transform ((to (eql 'lisp-emitting-form)) (input lisp-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-lisp-to-lisp-emitting-form input args))
