;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/string :in test))

(def definer string=-test (name args &body body)
  (labels ((process-entry (entry)
             (if (eq (first entry) 'with-expected-failures)
                 `(with-expected-failures
                    ,@(mapcar #'process-entry (rest entry)))
                 (bind (((expected form) entry))
                   `(is (string= ,expected ,form))))))
    `(def test ,name ,args
       ,@(mapcar #'process-entry body))))


(def string=-test test/string/simple ()
  ("this is a test"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
    ["this is a test"]}))

(def string=-test test/string/unquote ()
  ("this is a test"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
    ["this"
     ," is "
     "a test"]})
  ("this is a recursive test"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
    ["this"
     ,(list
       " is "
       ["a" " recursive"]
       " ")
     "test"]})
  ("this is a recursive test"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
    ["this"
     ,(concatenate 'string
                   " is "
                   (force-quasi-quoted-string ["a" " recursive"])
                   " ")
     "test"]}))
