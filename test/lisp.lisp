;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/lisp :in test))

(def test test/lisp/1 ()
  (is (equalp `(a b ,(list 1 2))
              {(with-transformed-quasi-quoted-syntax 'quasi-quoted-lisp 'lisp-emitting-form)
               `(a b ,(list 1 2))})))

(def test test/lisp/2 ()
  (is (equalp `(a b ,(list 1 `("call" 'me) 3))
              {(with-transformed-quasi-quoted-syntax 'quasi-quoted-lisp 'lisp-emitting-form)
               `(a b ,(list 1 `("call" 'me) 3))})))
