;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-binary (&key (start-character #\[)
                                         (end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (transformation nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (readtime-chain-transform transformation (make-binary-quasi-quote body)))
   (lambda (form spliced)
     (make-binary-unquote form spliced))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-binary-to-binary ()
  (set-quasi-quoted-binary-syntax-in-readtable :transformation '(binary)))

(define-syntax quasi-quoted-binary-to-binary-emitting-form ()
  (set-quasi-quoted-binary-syntax-in-readtable :transformation '(binary-emitting-form)))

;;;;;;;
;;; AST

(def (type e) binary ()
  '(vector (unsigned-byte 8)))

(def ast binary)

(def class* binary-syntax-node ()
  ())

(def (class* e) binary-quasi-quote (quasi-quote binary-syntax-node)
  ())

(def (function e) make-binary-quasi-quote (body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'binary-quasi-quote :body body))

(def (class* e) binary-unquote (unquote binary-syntax-node)
  ())

(def (function e) make-binary-unquote (form &optional (spliced? #f))
  (make-instance 'binary-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def function binary-position ()
  (file-position *quasi-quote-stream*))

(def function binary-concatenate (elements)
  (with-output-to-sequence (stream)
    (dolist (el elements)
      (write-sequence el stream))))

(def (macro e) with-binary-stream-to-binary (stream &body forms)
  `(with-output-to-sequence (,stream :element-type '(unsigned-byte 8))
     ,@forms))

(def function write-quasi-quoted-binary (node stream)
  (etypecase node
    (vector (write-sequence node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-binary node stream)) node))
    (function (funcall node)))
  (values))

(def function make-quasi-quoted-binary-emitting-form (node args)
  (etypecase node
    (binary `(write-sequence ,node *quasi-quote-stream*))
    (binary-unquote
     `(write-quasi-quoted-binary
       ,(apply #'transform-quasi-quoted-binary-to-binary-emitting-form node args) *quasi-quote-stream*))
    (side-effect (form-of node))))

(def function reduce-binary-subsequences (sequence)
  (reduce-subsequences sequence
                       (lambda (el) (typep el '(or (vector (not string)) binary)))
                       #'binary-concatenate))

(def function transform-quasi-quoted-binary-to-binary-emitting-form (input &rest args &key (properly-ordered #f) &allow-other-keys)
  (etypecase input
    (binary-quasi-quote
     (wrap-emitting-forms properly-ordered
                          (mapcar (lambda (node)
                                    (make-quasi-quoted-binary-emitting-form node args))
                                  (reduce-binary-subsequences (flatten (body-of input))))))
    (binary-unquote
     (map-filtered-tree (form-of input) 'binary-quasi-quote
                        (lambda (node)
                          (apply #'transform-quasi-quoted-binary-to-binary-emitting-form node args))))))

(def method transform ((to (eql 'binary-emitting-form)) (input binary-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-binary-to-binary-emitting-form input args))

(def method setup-emitting-environment ((to (eql 'binary-emitting-form)) &key stream-name next-method &allow-other-keys)
  (if stream-name
      (bind ((*quasi-quote-stream* (symbol-value stream-name)))
        (funcall next-method))
      (with-binary-stream-to-binary *quasi-quote-stream*
        (funcall next-method))))
