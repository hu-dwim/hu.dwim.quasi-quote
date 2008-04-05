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
        :cl-quasi-quote))

(defpackage :cl-quasi-quote-test-pdf
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-pdf
        ))

(dolist (package (list (find-package :cl-quasi-quote-test)
                       (find-package :cl-quasi-quote-test-pdf)))
  (import
   '(escape-as-xml)
   package))
