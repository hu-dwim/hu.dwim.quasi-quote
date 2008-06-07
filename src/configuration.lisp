;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defun transform-function-definer-options (options)
  (if cl-quasi-quote-system:*load-as-production-p*
      options
      (remove-from-plist options :inline :optimize)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-lambda-with-bang-args-syntax :start-character #\[ :end-character #\]))
