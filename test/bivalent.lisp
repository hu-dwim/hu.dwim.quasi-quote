;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-bivalent-to-binary-emitting-form-syntax)

(defsuite* (test/bivalent :in test))

(def binary=-test test/bivalent/simple ()
  ;; binary
  (#(1 2)
   [#(1 2)])

  (#(1 2 3 4)
    [#(1 2)
     #(3 4)])

  (#(1 2 3 4 5 6 7 8)
    [#(1 2)
      (#(3 4)
       #(5 6))
     #(7 8)])

  ;; string
  (#(49 32 50)
    ["1 2"])

  (#(49 32 50 32 51 32 52)
    ["1 2"
     " 3 4"])

  (#(49 32 50 32 51 32 52 32 53 32 54 32 55 32 56)
    ["1 2"
     (" 3 4" " 5 6")
     " 7 8"])

  ;; bivalent
  (#(1 2 49 32 50)
    [#(1 2) "1 2"])

  (#(1 2 3 4 53 32 54 32 55 32 56)
    [#(1 2)
     (#(3 4)
      "5 6")
     " 7 8"]))

(def binary=-test test/bivalent/unquote ()
  (#(1 2 3 4 32 53 32 54)
    [#(1 2)
     ,#(3 4)
     " 5 6"])

  (#(1 2 32 51 32 52 5 6 32 55 32 56 9 10 32 49 49 32 49 50)
    [#(1 2)
     ,(list
       " 3 4"
       [#(5 6) " 7 8"]
       #(9 10))
     " 11 12"]))
