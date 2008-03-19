;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-quasi-quote-test
  (:nicknames :qqt)

  (:use :common-lisp
        :metabang-bind
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote))

(in-package :cl-quasi-quote-test)

(in-root-suite)

(defsuite* test)
