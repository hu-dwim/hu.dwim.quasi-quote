;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/string :in test))

(def test test/string/1 ()
  (is (string= "this is a test"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
                   ["this is a test"]})))

(def test test/string/2 ()
  (is (string= "this is a test"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
                ["this"
                 ," is "
                 "a test"]})))

(def test test/string/3 ()
  (is (string= "this is a recursive test"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
                ["this"
                 ,(list
                   " is "
                   ["a" " recursive"]
                   " ")
                 "test"]})))

(def test test/string/4 ()
  (is (string= "this is a recursive test"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'string-emitting-form)
                ["this"
                 ,(concatenate 'string
                               " is "
                               (force-quasi-quoted-string ["a" " recursive"])
                               " ")
                 "test"]})))
