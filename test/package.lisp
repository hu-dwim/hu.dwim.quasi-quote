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
        :babel
        :babel-streams
        :cl-def
        :cl-walker
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-xml
        :cl-quasi-quote-pdf
        :cl-quasi-quote-ui
        :cl-quasi-quote-js)

  (:shadowing-import-from :cl-quasi-quote
   #:body-of
   #:parent-of
   #:form)
  
  (:export
   #:test))
