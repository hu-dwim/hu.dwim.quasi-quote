;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-test
  (:nicknames :qqt)

  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote)

  (:export
   #:test))

(defpackage :cl-quasi-quote-test-pdf
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-test
        :cl-quasi-quote-pdf
        ))

(defpackage :cl-quasi-quote-test-xml
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-test
        :cl-quasi-quote-xml
        ))

(dolist (package (mapcar 'find-package '(:cl-quasi-quote-test :cl-quasi-quote-test-pdf :cl-quasi-quote-test-xml)) )
  (import
   '()
   package))

