;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.quasi-quote.pdf
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.syntax-sugar))

(in-package :hu.dwim.quasi-quote.pdf)

(import-external-quasi-quote-symbols-for-extensions)

(defun setup-readtable ()
  (hu.dwim.quasi-quote::setup-readtable))
