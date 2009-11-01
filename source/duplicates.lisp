;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defun import-duplicate-symbols (&optional (package *package*))
  (import
   '(shrink-vector capitalize-first-letter capitalize-first-letter!)
   package))

(def (function io) make-adjustable-vector (initial-length &key (element-type t))
  (declare (type array-index initial-length))
  (make-array initial-length :adjustable t :fill-pointer 0 :element-type element-type))

(def (function io) shrink-vector (str &optional (size (length str)))
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

;; cl-l10n
(defun capitalize-first-letter (str)
  (if (and (> (length str) 0)
           (not (upper-case-p (elt str 0))))
      (capitalize-first-letter! (copy-seq str))
      str))

(defun capitalize-first-letter! (str)
  (setf (aref str 0) (char-upcase (aref str 0)))
  str)

(def function append* (&rest things)
  "Like append, but works for non-list arguments, too"
  (iter (for thing :in things)
        (nconcing (if (listp thing)
                      (copy-list thing)
                      (cons thing nil)))))
