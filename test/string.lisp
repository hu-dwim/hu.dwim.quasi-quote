;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-string-syntax)

(defsuite* (test/string :in test))

(def test-definer string)

(def special-variable *string-stream*)

(def function test-string-ast (expected ast)
  ;; evaluate to string
  (bind ((transformed (chain-transform '(string-emitting-form) ast)))
    (is (string= expected (qq::body-of (eval transformed)))))
  ;; write to string stream
  (bind ((transformed (chain-transform '((string-emitting-form :stream *string-stream*)) ast)))
    (is (string= expected
                 (bind ((*string-stream* (make-string-output-stream)))
                   (eval transformed)
                   (get-output-stream-string *string-stream*)))))
  ;; evaluate to binary
  (bind ((transformed (chain-transform '(quasi-quoted-binary binary-emitting-form) ast)))
    (is (string= expected (babel:octets-to-string (qq::body-of (eval transformed))))))
  ;; write to binary stream
  (bind ((transformed (chain-transform '(quasi-quoted-binary (binary-emitting-form :stream *string-stream*)) ast)))
    (is (string= expected
                 (bind ((*string-stream* (flexi-streams:make-in-memory-output-stream)))
                   (eval transformed)
                   (babel:octets-to-string (flexi-streams:get-output-stream-sequence *string-stream*)))))))

(def string-test test/string/simple ()
  ("1 2"
   ["1 2"])

  ("1 2 3 4"
   ["1 2"
    " 3 4"])

  ("1 2 3 4 5 6 7 8"
   ["1 2"
    (" 3 4"
     " 5 6")
    " 7 8"]))

(def string-test test/string/unquote ()
  ("1 2 3 4 5 6"
   ["1 2"
    ," 3 4 "
    "5 6"])

  ("1 2 3 4 5 6 7 8 9 10 11 12"
   ["1 2"
    ,(list
      " 3 4"
      [" 5 6" " 7 8"]
      " 9 10")
    " 11 12"])

  ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16"
   ["1 2"
    ,(list
      " 3 4"
      [" 5 6"
       ,(list " 7 8" " 9 10")
       " 11 12"]
      " 13 14")
    " 15 16"]))

(def string-test test/string/spliced-unquote ()
  ("1 2 3 4 5 6 7"
   ["1 "
    ,(make-string 1 :initial-element #\2)
    ,@(list " 3 " "4 " "5 ")
    ,(make-string 1 :initial-element #\6)
    " 7"]))
