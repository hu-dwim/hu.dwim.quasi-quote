;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-bivalent (&key (start-character #\[)
                                           (end-character #\])
                                           (unquote-character #\,)
                                           (splice-character #\@)
                                           (transformation nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (readtime-chain-transform transformation (make-bivalent-quasi-quote body)))
   (lambda (form spliced)
     (make-bivalent-unquote form spliced))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-bivalent-to-bivalent ()
  (set-quasi-quoted-bivalent-syntax-in-readtable :transformation '(bivalent)))

(define-syntax quasi-quoted-bivalent-to-bivalent-emitting-form ()
  (set-quasi-quoted-bivalent-syntax-in-readtable :transformation '(bivalent-emitting-form)))

(define-syntax quasi-quoted-bivalent-to-binary ()
  (set-quasi-quoted-bivalent-syntax-in-readtable :transformation '(quasi-quoted-binary binary)))

(define-syntax quasi-quoted-bivalent-to-binary-emitting-form ()
  (set-quasi-quoted-bivalent-syntax-in-readtable :transformation '(quasi-quoted-binary binary-emitting-form)))

;;;;;;;
;;; AST

(def ast bivalent)

(def class* bivalent-syntax-node ()
  ())

(def (class* e) bivalent-quasi-quote (quasi-quote bivalent-syntax-node)
  ())

(def (function e) make-bivalent-quasi-quote (body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'bivalent-quasi-quote :body body))

(def (class* e) bivalent-unquote (unquote bivalent-syntax-node)
  ())

(def (function e) make-bivalent-unquote (form &optional (spliced? #f))
  (make-instance 'bivalent-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def (macro e) with-bivalent-stream-to-binary (stream encoding &body forms)
  `(with-output-to-sequence (,stream :external-format (or ,encoding *default-character-encoding*))
     ,@forms))

(def function write-quasi-quoted-bivalent (node stream)
  (etypecase node
    (character (write-char node stream))
    (string (write-string node stream))
    (vector (write-sequence node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-bivalent node stream)) node))
    (function (funcall node)))
  (values))

(def function make-quasi-quoted-bivalent-emitting-form (node args)
  (etypecase node
    (binary `(write-sequence ,node *quasi-quote-stream*))
    (character `(write-char ,node *quasi-quote-stream*))
    (string
     (if (= 1 (length node))
         `(write-char ,(char node 0) *quasi-quote-stream*)
         `(write-string ,node *quasi-quote-stream*)))
    (bivalent-unquote
     `(write-quasi-quoted-bivalent
       ,(apply #'transform-quasi-quoted-bivalent-to-bivalent-emitting-form node args) *quasi-quote-stream*))
    (side-effect (form-of node))))

(def function transform-quasi-quoted-bivalent-to-bivalent-emitting-form (input &rest args &key (with-inline-emitting #f) &allow-other-keys)
  (etypecase input
    (bivalent-quasi-quote
     (wrap-emitting-forms with-inline-emitting
                          (mapcar (lambda (node)
                                    (make-quasi-quoted-bivalent-emitting-form node args))
                                  (reduce-binary-subsequences (reduce-string-subsequences (flatten (body-of input)))))))
    (bivalent-unquote
     (map-filtered-tree (form-of input) 'bivalent-quasi-quote
                        (lambda (node)
                          (apply #'transform-quasi-quoted-bivalent-to-bivalent-emitting-form node args))))))

(def method transform ((to (eql 'bivalent-emitting-form)) (input bivalent-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-bivalent-to-bivalent-emitting-form input args))

(def method setup-emitting-environment ((to (eql 'bivalent-emitting-form)) &key stream-name encoding next-method &allow-other-keys)
  (if stream-name
      (bind ((*quasi-quote-stream* (symbol-value stream-name)))
        (funcall next-method))
      (with-bivalent-stream-to-binary *quasi-quote-stream* encoding
        (funcall next-method))))

(def function transform-quasi-quoted-bivalent-to-quasi-quoted-binary (node &rest args &key (encoding *default-character-encoding*) &allow-other-keys)
  (etypecase node
    (function node)
    (list
     (mapcar (lambda (child)
               (apply #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary child args))
             node))
    (character (babel:string-to-octets (string node) :encoding encoding)) ;; TODO: more efficient way
    (string (babel:string-to-octets node :encoding encoding))
    (vector (coerce node 'binary))
    (bivalent-quasi-quote
     (make-binary-quasi-quote (apply #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary (body-of node) args)))
    (bivalent-unquote
     (make-binary-unquote
      `(transform-quasi-quoted-bivalent-to-quasi-quoted-binary
        ,(map-filtered-tree (form-of node) 'bivalent-quasi-quote
                            (lambda (child)
                              (apply #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary child args)))
        :encoding ,encoding)))
    (quasi-quote
     (if (typep node 'binary-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-binary node))
    (side-effect node)))

(def method transform ((to (eql 'quasi-quoted-binary)) (input bivalent-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-bivalent-to-quasi-quoted-binary input args))
