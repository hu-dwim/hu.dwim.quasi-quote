;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

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

(defun import-duplicate-symbols (&optional (package *package*))
  (import
   '(shrink-vector)
   package))
