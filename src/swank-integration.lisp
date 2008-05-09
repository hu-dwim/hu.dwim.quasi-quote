;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(register-readtable-for-swank
 '("CL-QUASI-QUOTE") 'setup-readtable)
