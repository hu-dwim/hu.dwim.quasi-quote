;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-quasi-quote-pdf
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote)

  (:export))

(in-package :cl-quasi-quote-pdf)

(import-external-quasi-quote-symbols-for-extensions)

(defun transform-function-definer-options (options)
  (cl-quasi-quote::transform-function-definer-options options))

(defun setup-readtable ()
  (cl-quasi-quote::setup-readtable))
