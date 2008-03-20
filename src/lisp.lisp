;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-lisp (&key (quasi-quote-character #\`)
                                       (quasi-quote-end-character nil)
                                       (unquote-character #\,)
                                       (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body) (make-instance 'lisp-quasi-quote :body body))
   (lambda (form spliced) (make-instance 'lisp-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

;;;;;;;
;;; AST

(def ast lisp)

(def class* lisp-syntax-node (syntax-node)
  ((name)))

(def class* lisp-quasi-quote (quasi-quote lisp-syntax-node)
  ())

(def class* lisp-unquote (unquote lisp-syntax-node)
  ())

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'lisp-emitting-form)) (input lisp-syntax-node) &key &allow-other-keys)
  (etypecase input
    (lisp-quasi-quote
     (labels ((process (node)
                (etypecase node
                  (lisp-unquote (transform 'lisp-emitting-form node))
                  (list `(list ,@(mapcar #'process node)))
                  (t (list 'quote node)))))
       (process (body-of input))))
    (lisp-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'lisp-quasi-quote)
                     (transform 'lisp-emitting-form form)
                     form))))))
