;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-test
  (:nicknames :qqt)

  (:use :common-lisp
        :metabang-bind
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote))

(import
 '(escape-as-xml)
 (find-package :cl-quasi-quote-test))

(in-package :cl-quasi-quote-test)

(in-root-suite)

(defsuite* test)
