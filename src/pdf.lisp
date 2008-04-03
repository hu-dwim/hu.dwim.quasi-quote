;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-pdf (&key (quasi-quote-character #\[)
                                      (quasi-quote-end-character #\])
                                      (unquote-character #\,)
                                      (splice-character #\@)
                                      (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-pdf-quasi-quote :body (parse-quasi-quoted-pdf body)))))
   (lambda (form spliced)
     (make-pdf-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(defgeneric parse-quasi-quoted-pdf (form)
  )

;;;;;;;
;;; AST

(def ast pdf)

(def class* pdf-syntax-node (syntax-node)
  ())

(def (class* e) pdf-quasi-quote (quasi-quote pdf-syntax-node)
  ())

(def (function e) make-pdf-quasi-quote (body)
  (make-instance 'pdf-quasi-quote :body body))

(def (class* e) pdf-unquote (unquote pdf-syntax-node)
  ())

(def (function e) make-pdf-unquote (form &optional (spliced? #f))
  (make-instance 'pdf-unquote :form form :spliced spliced?))

(def (class*) pdf-document (pdf-syntax-node)
  ())

(def (class*) pdf-page (pdf-syntax-node)
  ())

(def (class*) pdf-string (pdf-syntax-node)
  ())

;;;;;;;;;;;;;
;;; Transform

(def function transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (node &rest args &key &allow-other-keys)
  (etypecase node
    (function node)
    (pdf-quasi-quote
     (make-bivalent-quasi-quote (apply #'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (body-of node) args)))
    (pdf-unquote
     (make-bivalent-unquote
      `(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent
        ,(map-filtered-tree (form-of node) 'pdf-quasi-quote
                            (lambda (child)
                              (apply #'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent child args))))))
    (quasi-quote
     (if (typep node 'bivalent-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-binary node))))

(def method transform ((to (eql 'quasi-quoted-bivalent)) (input pdf-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent input args))
