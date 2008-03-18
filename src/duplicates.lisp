;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

;; dwim util
#+#.(cl:when (cl:find-package "SWANK") '(:and))
(defun setup-swank-readtable-alist (&rest package-name/readtable-setup-function-pairs)
  (loop for (package-names setup-function) :on package-name/readtable-setup-function-pairs :by #'cddr do
        (bind ((*readtable* (copy-readtable)))
          (funcall setup-function)
          (dolist (package-name (ensure-list package-names))
            (setf package-name (string package-name))
            (let ((entry (find package-name swank:*readtable-alist* :test #'string= :key #'car)))
              (unless entry
                (setf entry (cons package-name nil))
                (push entry swank:*readtable-alist*))
              (setf (cdr entry) *readtable*))))))

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for t and nil in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil)))
