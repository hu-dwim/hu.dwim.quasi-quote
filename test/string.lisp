;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/string :in test))

(def function with-quasi-quoted-string ()
  (lambda (reader)
    (set-quasi-quote-syntax-in-readtable
     (lambda (body) (make-instance 'quasi-quote :body body))
     (lambda (form spliced) (make-instance 'unquote :form form :spliced spliced))
     :quasi-quote-character #\[
     :quasi-quote-end-character #\])
    (first (funcall reader))))

(def test test/string/1 ()
  (is (string= "this is a test"
               (transform-quasi-quoted-string-to-string
                {with-quasi-quoted-string
                    ["this is a test"]}))))

(def test test/string/2 ()
  (is (string= "this is a test"
               (transform-quasi-quoted-string-to-string
                {with-quasi-quoted-string
                    ["this"
                     ," is "
                     "a test"]}))))

(def test test/string/3 ()
  (is (string= "this is a recursive test"
               (transform-quasi-quoted-string-to-string
                {with-quasi-quoted-string
                    ["this"
                     ,(list
                       " is "
                       ["a" " recursive"]
                       " ")
                     "test"]}))))

(def test test/string/4 ()
  (is (string= "this is a recursive test"
               (transform-quasi-quoted-string-to-string
                {with-quasi-quoted-string
                    ["this"
                     ,(concatenate 'string
                                   " is "
                                   (force-quasi-quoted-string ["a" " recursive"])
                                   " ")
                     "test"]}))))
