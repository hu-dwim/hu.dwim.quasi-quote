;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defun import-duplicate-symbols (&optional (package *package*))
  (import
   '(shrink-vector if-bind aif when-bind awhen prog1-bind aprog1 it capitalize-first-letter capitalize-first-letter!
     handle-otherwise)
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

;; cl-l10n
(defun capitalize-first-letter (str)
  (if (and (> (length str) 0)
           (not (upper-case-p (elt str 0))))
      (capitalize-first-letter! (copy-seq str))
      str))

(defun capitalize-first-letter! (str)
  (setf (aref str 0) (char-upcase (aref str 0)))
  str)

;; wui
(def (function io) handle-otherwise (otherwise)
  (cond
    ((eq otherwise :error)
     (error "Otherwise assertion failed"))
    ((and (consp otherwise)
          (member (first otherwise) '(:error :warn)))
     (case (first otherwise)
       (:error (apply #'error (second otherwise) (nthcdr 2 otherwise)))
       (:warn (apply #'warn (second otherwise) (nthcdr 2 otherwise)))))
    ((functionp otherwise)
     (funcall otherwise))
    (t
     otherwise)))

(def function not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate 'string "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(def function append* (&rest things)
  "Like append, but works for non-list arguments, too"
  (iter (for thing :in things)
        (nconcing (if (listp thing)
                      (copy-list thing)
                      (cons thing nil)))))