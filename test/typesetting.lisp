;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/typesetting :in test))

(def test test/typesetting/1 ()
  (is (string= "<div/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'string-emitting-form)
                [(typesetting-list)]})))
