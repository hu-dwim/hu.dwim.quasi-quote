;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defun import-duplicate-symbols (&optional (package *package*))
  (import
   '(shrink-vector if-bind aif when-bind awhen prog1-bind aprog1 it)
   package))

(defun shrink-vector (str size)
  "Only callable on local vectors with fill pointer"
  #+allegro
  (excl::.primcall 'sys::shrink-svector str size)
  #+sbcl
  (setq str (sb-kernel:%shrink-vector str size))
  #+cmu
  (lisp::shrink-vector str size)
  #+lispworks
  (system::shrink-vector$vector str size)
  #+scl
  (common-lisp::shrink-vector str size)
  #-(or allegro cmu lispworks sbcl scl)
  (setq str (subseq str 0 size))
  str)

(defmacro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))
