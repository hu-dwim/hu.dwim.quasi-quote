;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/binary :in test))

(def test test/binary/1 ()
  (is (equalp #(1 2 3)
              {(with-transformed-quasi-quoted-syntax 'quasi-quoted-binary 'binary-emitting-form)
               [#(1 2 3)]})))
