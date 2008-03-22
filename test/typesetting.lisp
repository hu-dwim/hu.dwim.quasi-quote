;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/typesetting :in test))

(def test test/typesetting/1 ()
  (is (string= "<div/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'string-emitting-form)
                [vertical-list]})))

(def test test/typesetting/2 ()
  (is (string= "<span>Hello World</span>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'string-emitting-form)
                [paragraph "Hello World"]})))

(def test test/typesetting/3 ()
  (is (string= "<ul><li>1</li><li>2</li></ul>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'string-emitting-form)
                [menu
                 (menu-item "1")
                 (menu-item "2")]})))
