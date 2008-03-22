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
                                         (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (make-instance 'binary-quasi-quote
                    :body (if (and (consp body)
                                   (vectorp (first body)))
                              (list (coerce (first body) '(simple-array (unsigned-byte 8) (*))))
                              body)))
   (lambda (form spliced)
     (make-instance 'binary-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

;;;;;;;
;;; AST

(def ast binary)

(def class* binary-syntax-node ()
  ())

(def (class* e) binary-quasi-quote (quasi-quote binary-syntax-node)
  ())

(def (class* e) binary-unquote (unquote binary-syntax-node)
  ())

;;;;;;;;;;;;;
;;; Transform

(def special-variable *binary-stream*)

(def function write-quasi-quoted-binary (node)
  (etypecase node
    ((vector (unsigned-byte 8)) (write-sequence node *binary-stream*))
    (list (mapc #'write-quasi-quoted-binary node))
    (function (funcall node)))
  (values))

(def macro with-quasi-quoted-binary-emitting-environment (&body forms)
  `(bind ((*binary-stream* (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
     ,@forms
     (flexi-streams:get-output-stream-sequence *binary-stream*)))

(def (macro e) force-quasi-quoted-binary (node)
  `(with-quasi-quoted-binary-emitting-environment
     (write-quasi-quoted-binary ,node)))

(def method transform ((to (eql 'binary)) (input binary-syntax-node) &rest args &key &allow-other-keys)
  (funcall (apply #'transform 'binary-emitting-lambda input args)))

(def method transform ((to (eql 'binary-emitting-form)) (input binary-syntax-node) &key (toplevel #t) (stream '*binary-stream* stream-provided?) &allow-other-keys)
  (etypecase input
    (binary-quasi-quote
     (labels ((process (node)
                (etypecase node
                  ((vector (unsigned-byte 8)) `(write-sequence ,node ,stream))
                  (binary-unquote `(write-quasi-quoted-binary ,(transform 'binary-emitting-form node :stream stream)))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((forms (reduce-subsequences (flatten (body-of input))
                                          #'vectorp
                                          (lambda (&rest elements)
                                            (apply #'concatenate '(vector (unsigned-byte 8)) elements))))
              (processed-forms (if (and toplevel
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms))))
         (if (and toplevel
                  (not (single-string-list-p processed-forms))
                  (not stream-provided?))
             `(with-quasi-quoted-binary-emitting-environment
                ,@processed-forms)
             `(progn
                ,@processed-forms)))))
    (binary-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'quasi-quote)
                     (transform 'binary-emitting-lambda-form form :toplevel #f :stream stream)
                     form))))))
