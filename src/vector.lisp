;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def special-variable *binary-stream*)

(def function write-quasi-quoted-binary (node)
  (etypecase node
    (vector (write-sequence node *binary-stream*))
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

(def (function e) transform-quasi-quoted-binary-to-form (qq-binary &key (toplevel #t) (stream '*binary-stream*))
  (etypecase qq-binary
    (quasi-quote
     (labels ((process (node)
                (etypecase node
                  ((vector (unsigned-byte 8)) `(write-sequence ,node ,stream))
                  (unquote `(write-quasi-quoted-binary (funcall ,(transform-quasi-quoted-binary-to-lambda-form node))))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((toplevel (or toplevel
                            (toplevel-p qq-binary)))
              (forms (reduce-subsequences (flatten (body-of qq-binary))
                                          #'stringp
                                          (lambda (&rest elements)
                                            (apply #'concatenate 'string elements))))
              (processed-forms (if (and toplevel
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms))))
         (if (and toplevel
                  (not (single-string-list-p processed-forms)))
             `(with-quasi-quoted-binary-emitting-environment
                ,@processed-forms)
             `(progn
                ,@processed-forms)))))
    (unquote
     (labels ((process (form)
                (cond ((typep form 'quasi-quote)
                       (if (toplevel-p form)
                           `(funcall ,(expand-quasi-quoted-string-to-lambda-form form (toplevel-p form)))
                           (expand-quasi-quoted-string-to-lambda-form form #f)))
                      ((consp form)
                       (cons (process (car form))
                             (process (cdr form))))
                      (t
                       form))))
       (process (form-of qq-binary))))))

(def (function e) transform-quasi-quoted-binary-to-lambda-form (qq-binary)
  `(lambda () ,(transform-quasi-quoted-binary-to-form qq-binary)))

(def (function e) transform-quasi-quoted-binary-to-binary (qq-binary)
  (funcall (compile nil (transform-quasi-quoted-binary-to-lambda-form qq-binary))))
