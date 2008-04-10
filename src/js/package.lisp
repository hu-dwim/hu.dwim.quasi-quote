;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-js
  (:use :common-lisp
        :cl-syntax-sugar
        :cl-quasi-quote
        :metabang-bind
        :alexandria
        :iterate
        :cl-def
        :cl-walker
        )

  (:shadowing-import-from :cl-quasi-quote
   #:body-of
   #:parent-of)

  (:export
   ))

(in-package :cl-quasi-quote-js)

(cl-quasi-quote::import-duplicate-symbols)
(cl-quasi-quote::import-semi-external-quasi-quote-symbols)

(defun transform-function-definer-options (options)
  (cl-quasi-quote::transform-function-definer-options options))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '("CL-QUASI-QUOTE-JS") 'setup-readtable)