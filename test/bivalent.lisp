;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-bivalent-syntax)

(defsuite* (test/bivalent :in test))

(def test-definer bivalent)

(def special-variable *bivalent-stream*)

(def function test-bivalent-ast (expected ast)
  ;; evaluate to binary
  (bind ((transformed (chain-transform '(quasi-quoted-binary binary-emitting-form) ast)))
    (is (equalp expected (qq::body-of (eval transformed)))))
  ;; write to binary stream
  (bind ((transformed (chain-transform '(quasi-quoted-binary (binary-emitting-form :stream *bivalent-stream*)) ast)))
    (is (equalp expected
                (bind ((*bivalent-stream* (flexi-streams:make-in-memory-output-stream)))
                  (eval transformed)
                  (flexi-streams:get-output-stream-sequence *bivalent-stream*))))))

(def bivalent-test test/bivalent/simple ()
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

(def bivalent-test test/bivalent/unquote ()
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
     " 11 12"])

  (#(1 2 32 51 32 52 5 6 7 8 32 57 32 49 48 32 49 49 32 49 50 13 14 32 49 53 32 49 54)
    [#(1 2)
     ,(list
       " 3 4"
       [#(5 6)
        ,(list #(7 8) " 9 10")
        " 11 12"]
       #(13 14))
     " 15 16"]))
