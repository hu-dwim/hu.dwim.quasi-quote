;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-string (&key (start-character #\[)
                                         (end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (readtime-chain-transform transform (make-string-quasi-quote body)))
   (lambda (form spliced)
     (make-string-unquote form spliced))
   :start-character start-character
   :end-character end-character
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
;;; A quasi quoted string is made of character, string, list, string-quasi-quote, string-unquote nodes recursively.

(def ast string)

(def class* string-syntax-node (syntax-node)
  ())

(def (class* e) string-quasi-quote (quasi-quote string-syntax-node)
  ())

(def (function e) make-string-quasi-quote (body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'string-quasi-quote :body body))

(def (class* e) string-unquote (unquote string-syntax-node)
  ())

(def (function e) make-string-unquote (form &optional (spliced? #f))
  (make-instance 'string-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def function string-concatenate (elements)
  (bind ((*print-pretty* #f)
         (*print-readably* #f))
    (with-output-to-string (stream)
      (dolist (el elements)
        (etypecase el
          (string (write-string el stream))
          (character (write-char el stream)))))))

(def macro with-string-stream-to-string (stream &body forms)
  `(bind ((,stream (make-string-output-stream)))
     ,@forms
     (get-output-stream-string ,stream)))

(def function write-quasi-quoted-string (node stream)
  (etypecase node
    (character (write-char node stream))
    (string (write-string node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-string node stream)) node))
    (function (funcall node)))
  (values))

(def function make-quasi-quoted-string-emitting-form (node)
  (etypecase node
    (character `(write-char ,node *quasi-quote-stream*))
    (string
     (if (= 1 (length node))
         `(write-char ,(char node 0) *quasi-quote-stream*)
         `(write-string ,node *quasi-quote-stream*)))
    (string-unquote
     `(write-quasi-quoted-string ,(transform-quasi-quoted-string-to-string-emitting-form node :toplevel #f) *quasi-quote-stream*))
    (side-effect (form-of node))))

(def function reduce-string-subsequences (sequence)
  (reduce-subsequences sequence
                       (lambda (el) (typep el '(or character string)))
                       #'string-concatenate))

(def function transform-quasi-quoted-string-to-string-emitting-form (input &key &allow-other-keys)
  (etypecase input
    (string-quasi-quote
     (wrap-forms-with-lambda
      (append (mapcar #'make-quasi-quoted-string-emitting-form
                      (reduce-string-subsequences (flatten (body-of input))))
              '((values)))))
    (string-unquote
     (map-filtered-tree (form-of input) 'string-quasi-quote
                        #'transform-quasi-quoted-string-to-string-emitting-form))))

(def method transform ((to (eql 'string-emitting-form)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-string-emitting-form input args))

(def method setup-emitting-environment ((to (eql 'string-emitting-form)) &key stream-name next-method &allow-other-keys)
  (if stream-name
      (bind ((*quasi-quote-stream* (symbol-value stream-name)))
        (funcall next-method))
      (with-string-stream-to-string *quasi-quote-stream*
        (funcall next-method))))

(def function transform-quasi-quoted-string-to-quasi-quoted-binary (node &rest args &key (encoding :utf-8) &allow-other-keys)
  (etypecase node
    (function node)
    (list
     (mapcar (lambda (child)
               (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary child args))
             node))
    (character (babel:string-to-octets (string node) :encoding encoding)) ;; TODO: more efficient way
    (string (babel:string-to-octets node :encoding encoding))
    (string-quasi-quote
     (make-binary-quasi-quote (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary (body-of node) args)))
    (string-unquote
     (make-binary-unquote
      `(transform-quasi-quoted-string-to-quasi-quoted-binary
        ,(map-filtered-tree (form-of node) 'string-quasi-quote
                            (lambda (child)
                              (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary child args)))
        :encoding ,encoding)))
    (quasi-quote
     (if (typep node 'binary-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-binary node))
    (side-effect node)))

(def method transform ((to (eql 'quasi-quoted-binary)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary input args))
