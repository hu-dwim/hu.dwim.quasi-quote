;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-string-syntax)

(defsuite* (test/string :in test))

(def test-definer string)

(def special-variable *string-stream*)

(def function test-string-ast (expected ast)
  ;; evaluate to string
  (is (string= expected
               (transform-and-emit '(string-emitting-form
                                     lambda-form
                                     lambda)
                                   ast)))
  ;; write to string stream
  (is (string= expected
               (bind ((*string-stream* (make-string-output-stream)))
                 (transform-and-emit '((string-emitting-form :stream-name *string-stream*)
                                       lambda-form
                                       lambda)
                                     ast)
                 (get-output-stream-string *string-stream*))))
  ;; evaluate to binary
  (is (string= expected
               (babel:octets-to-string
                (transform-and-emit '(quasi-quoted-binary
                                      binary-emitting-form
                                      lambda-form
                                      lambda)
                                    ast))))
  ;; write to binary stream
  (is (string= expected
               (with-output-to-sequence (*string-stream* :return-as 'string)
                 (transform-and-emit '(quasi-quoted-binary
                                       (binary-emitting-form :stream-name *string-stream*)
                                       lambda-form
                                       lambda)
                                     ast)))))

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

(def string-test test/string/reverse ()
  ("1 2 3 4 5 6 7 8"
   ["1 2"
    ,(reverse
      (list [" 5 6"] [" 3 4"]))
    " 7 8"]))
