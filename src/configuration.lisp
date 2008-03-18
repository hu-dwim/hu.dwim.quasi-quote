;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(enable-sharp-boolean-syntax)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defun setup-readtable ()
  (enable-sharp-boolean-syntax))

(defun file-header ()
  `(eval-always
    (setup-readtable)
    (values)))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(setup-swank-readtable-alist
 '("CL-QUASI-QUOTE" "CL-QUASI-QUOTE-TEST") 'setup-readtable)
