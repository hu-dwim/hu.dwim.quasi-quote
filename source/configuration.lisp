;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def function setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-lambda-with-bang-args-syntax))
