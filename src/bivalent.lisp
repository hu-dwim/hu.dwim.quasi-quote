;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-bivalent (&key (quasi-quote-character #\[)
                                           (quasi-quote-end-character #\])
                                           (unquote-character #\,)
                                           (splice-character #\@)
                                           (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-bivalent-quasi-quote body))))
   (lambda (form spliced)
     (make-bivalent-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-bivalent-to-binary ()
  (set-quasi-quoted-bivalent-syntax-in-readtable :transform '(quasi-quoted-binary binary)))

(define-syntax quasi-quoted-bivalent-to-binary-emitting-form ()
  (set-quasi-quoted-bivalent-syntax-in-readtable :transform '(quasi-quoted-binary binary-emitting-form)))

;;;;;;;
;;; AST

(def ast bivalent)

(def class* bivalent-syntax-node ()
  ())

(def (class* e) bivalent-quasi-quote (quasi-quote bivalent-syntax-node)
  ())

(def (function e) make-bivalent-quasi-quote (body)
  (make-instance 'bivalent-quasi-quote :body body))

(def (class* e) bivalent-unquote (unquote bivalent-syntax-node)
  ())

(def (function e) make-bivalent-unquote (form &optional (spliced? #f))
  (make-instance 'bivalent-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def function transform-quasi-quoted-bivalent-to-quasi-quoted-binary (node &key (encoding :utf-8) &allow-other-keys)
  (etypecase node
    (void-syntax-node node)
    (function node)
    (list (mapcar #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary node))
    (string (babel:string-to-octets node :encoding encoding))
    (vector (coerce node 'binary))
    (bivalent-quasi-quote
     (make-binary-quasi-quote (transform-quasi-quoted-bivalent-to-quasi-quoted-binary (body-of node))))
    (bivalent-unquote
     (make-binary-unquote
      `(transform-quasi-quoted-bivalent-to-quasi-quoted-binary
        ,(map-filtered-tree (form-of node) 'bivalent-quasi-quote #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary))))
    (quasi-quote (body-of (transform 'quasi-quoted-binary node)))
    (unquote (transform 'quasi-quoted-binary node))))

(def method transform ((to (eql 'quasi-quoted-binary)) (input bivalent-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary input args))
