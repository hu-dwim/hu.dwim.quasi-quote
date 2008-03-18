;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/string :in test))

(def test test/string/1 ()
  (is (string= "this is a test"
               (transform-quasi-quoted-string-to-string
                (make-instance 'quasi-quote
                               :body "this is a test")))))

(def test test/string/2 ()
  (is (string= "this is a test"
               (transform-quasi-quoted-string-to-string
                (make-instance 'quasi-quote
                               :body `("this"
                                       ,(make-instance 'unquote
                                                       :form " is ")
                                       "a test"))))))

(def test test/string/3 ()
  (is (string= "this is a recursive test"
               (transform-quasi-quoted-string-to-string
                (make-instance 'quasi-quote
                               :body `("this"
                                       ,(make-instance 'unquote
                                                       :form `(list
                                                               " is "
                                                               ,(make-instance 'quasi-quote
                                                                               :body '("a" " recursive"))
                                                               " "))
                                       "test"))))))

(def test test/string/4 ()
  (is (string= "this is a recursive test"
               (transform-quasi-quoted-string-to-string
                (make-instance 'quasi-quote
                               :body `("this"
                                       ,(make-instance 'unquote
                                                       :form `(concatenate 'string
                                                                           " is "
                                                                           ,(make-instance 'quasi-quote
                                                                                           :toplevel #t
                                                                                           :body '("a" " recursive"))
                                                                           " "))
                                       "test"))))))
