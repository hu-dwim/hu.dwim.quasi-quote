;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/lisp :in test))

(def test-definer lisp)

(def function test-lisp-ast (expected ast)
  ;; evaluate to lisp
  (bind ((transformed (chain-transform '(lisp-emitting-form) ast)))
    (is (equalp expected (qq::body-of (eval transformed))))))

(def lisp-test test/lisp/simple ()
  (`(a b)
    {with-quasi-quoted-lisp-syntax
        `(a b)})

  (`(a b (1 2))
    {with-quasi-quoted-lisp-syntax
        `(a b (1 2))}))

(def lisp-test test/lisp/unquote ()
  (`(a b ,(list 1 2))
    {with-quasi-quoted-lisp-syntax
        `(a b ,(list 1 2))})

  (`(a b ,(list 1 `("call" 'me) 3))
    {with-quasi-quoted-lisp-syntax
        `(a b ,(list 1 `("call" 'me) 3))})

  (`(a b ,(list 1 `("call" 'me ,(list "Al")) 3))
    {with-quasi-quoted-lisp-syntax
        `(a b ,(list 1 `("call" 'me ,(list "Al")) 3))}))
