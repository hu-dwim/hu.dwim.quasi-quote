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
        :flexi-streams
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
        :flexi-streams
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-test
        :cl-quasi-quote-pdf))

(defpackage :cl-quasi-quote-test-xml
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :flexi-streams
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-test
        :cl-quasi-quote-xml))

(defpackage :cl-quasi-quote-test-ui
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :flexi-streams
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-test
        :cl-quasi-quote-ui
        :cl-quasi-quote-xml
        :cl-quasi-quote-pdf))

(defpackage :cl-quasi-quote-test-js
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :flexi-streams
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-test
        :cl-quasi-quote-js
        :cl-walker)
  
  (:shadowing-import-from :cl-quasi-quote
   #:body-of
   #:parent-of
   ))

(dolist (package (mapcar 'find-package '(:cl-quasi-quote-test :cl-quasi-quote-test-pdf :cl-quasi-quote-test-xml
                                         :cl-quasi-quote-test-js)))
  (import
   '()
   package))
