;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-typesetting-to-string-emitting-form-syntax)

(defsuite* (test/typesetting :in test))

(def string=-test test/typesetting/1 ()
  ("<table/>"
   [vertical-list]))
