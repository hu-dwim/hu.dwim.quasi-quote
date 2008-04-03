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
       (readtime-chain-transform transform (make-pdf-quasi-quote (parse-quasi-quoted-pdf body)))))
   (lambda (form spliced)
     (make-pdf-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-pdf-to-binary ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary)))

(define-syntax quasi-quoted-pdf-to-binary-emitting-form ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-pdf-to-binary-stream-emitting-form (stream)
  (set-quasi-quoted-pdf-syntax-in-readtable :transform `(quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream ,stream))))

(def function pdf-syntax-node-name (name)
  (format-symbol (find-package :cl-quasi-quote) "PDF-~A" name))

(def function parse-quasi-quoted-pdf (form)
  (if (typep form 'syntax-node)
      form
      (parse-quasi-quoted-pdf* (pdf-syntax-node-name (first form)) form)))

(defgeneric parse-quasi-quoted-pdf* (first whole)
  (:method ((first (eql 'pdf-document)) whole)
    (make-instance 'pdf-document :elements (mapcar #'parse-quasi-quoted-pdf (cdr whole)))))

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
  ((elements)))

(def (class*) pdf-page (pdf-syntax-node)
  ())

(def (class*) pdf-string (pdf-syntax-node)
  ())

;;;;;;;;;;;;;
;;; Transform

(defgeneric transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (node)
  (:method ((node function))
    node)

  (:method ((node quasi-quote))
    (if (typep node 'bivalent-quasi-quote)
        (body-of node)
        node))
  
  (:method ((node unquote))
    (transform 'quasi-quoted-binary node))

  (:method ((node pdf-quasi-quote))
    (make-bivalent-quasi-quote (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (body-of node))))

  (:method ((node pdf-unquote))
    (make-bivalent-unquote
     `(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent
       ,(map-filtered-tree (form-of node) 'pdf-quasi-quote #'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent))))

  (:method ((node pdf-document))
    `("%PDF-1.4" ,(mapcar #'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (elements-of node)))))

(def method transform ((to (eql 'quasi-quoted-bivalent)) (input pdf-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent input))
