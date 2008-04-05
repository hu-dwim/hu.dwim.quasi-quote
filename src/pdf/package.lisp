;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-quasi-quote-pdf
  (:use :common-lisp
        :cl-syntax-sugar
        :cl-quasi-quote
        :metabang-bind
        :alexandria
        :iterate
        :cl-def
        )

  (:export))

(in-package :cl-quasi-quote-pdf)

(cl-quasi-quote::import-duplicate-symbols)
(cl-quasi-quote::import-semi-external-quasi-quote-symbols)

(defun transform-function-definer-options (options)
  (cl-quasi-quote::transform-function-definer-options options))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '("CL-QUASI-QUOTE-PDF") 'setup-readtable)