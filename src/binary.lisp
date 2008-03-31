;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-binary (&key (quasi-quote-character #\[)
                                         (quasi-quote-end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-binary-quasi-quote body))))
   (lambda (form spliced)
     (make-binary-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-binary-to-binary ()
  (set-quasi-quoted-binary-syntax-in-readtable :transform '(binary)))

(define-syntax quasi-quoted-binary-to-binary-emitting-form ()
  (set-quasi-quoted-binary-syntax-in-readtable :transform '(binary-emitting-form)))

;;;;;;;
;;; AST

(def ast binary)

(def class* binary-syntax-node ()
  ())

(def (class* e) binary-quasi-quote (quasi-quote binary-syntax-node)
  ())

(def (function e) make-binary-quasi-quote (body)
  (make-instance 'binary-quasi-quote :body body))

(def (class* e) binary-unquote (unquote binary-syntax-node)
  ())

(def (function e) make-binary-unquote (form &optional (spliced? #f))
  (make-instance 'binary-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def special-variable *binary-stream*)

(def function write-quasi-quoted-binary (node stream)
  (etypecase node
    (vector (write-sequence node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-binary node stream)) node))
    (binary-quasi-quote (write-quasi-quoted-binary (body-of node) stream))
    (function (funcall node)))
  (values))

(def macro with-quasi-quoted-binary-emitting-environment (&body forms)
  `(bind ((*binary-stream* (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
     ,@forms
     (flexi-streams:get-output-stream-sequence *binary-stream*)))

(def (macro e) force-quasi-quoted-binary (node)
  `(with-quasi-quoted-binary-emitting-environment
     (write-quasi-quoted-binary ,node *binary-stream*)))

(def function transform-quasi-quoted-binary-to-binary-emitting-form (input &key (toplevel #t) (stream '*binary-stream*) &allow-other-keys)
  (etypecase input
    (binary-quasi-quote
     (labels ((process (node)
                (etypecase node
                  ((vector (unsigned-byte 8)) `(write-sequence ,node ,stream))
                  (binary-unquote `(write-quasi-quoted-binary ,(transform 'binary-emitting-form node :toplevel #f :stream stream) ,stream))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((forms (reduce-subsequences (flatten (body-of input))
                                          #'vectorp
                                          (lambda (&rest elements)
                                            (apply #'concatenate '(vector (unsigned-byte 8)) elements))))
              (internal-stream? (eq stream '*binary-stream*))
              (processed-forms (if (and toplevel
                                        internal-stream?
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms))))
         (if (and toplevel
                  internal-stream?
                  (not (single-string-list-p processed-forms)))
             `(with-quasi-quoted-binary-emitting-environment
                ,@processed-forms)
             `(progn
                ,@processed-forms
                ,@(unless internal-stream?
                          `((values))))))))
    (binary-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'quasi-quote)
                     (transform 'binary-emitting-form form :toplevel #f :stream stream)
                     form))))))

(def method transform ((to (eql 'binary-emitting-form)) (input binary-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-binary-to-binary-emitting-form input args))
