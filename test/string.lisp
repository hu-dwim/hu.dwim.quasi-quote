;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-string-to-string-emitting-form-syntax)

(defsuite* (test/string :in test))

(def string=-test test/string/simple ()
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

(def string=-test test/string/unquote ()
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

  ("1 2 3 4 5 6 7 8 9 10 11 12"
   ["1 2"
    ,(concatenate 'string
                  " 3 4"
                  (force-quasi-quoted-string [" 5 6" " 7 8"])
                  " 9 10")
    " 11 12"]))

(def string=-test test/string/spliced-unquote ()
  ("1 2 3 4 5 6 7"
   ["1 "
    ,(make-string 1 :initial-element #\2)
    ,@(list " 3 " "4 " "5 ")
    ,(make-string 1 :initial-element #\6)
    " 7"]))
