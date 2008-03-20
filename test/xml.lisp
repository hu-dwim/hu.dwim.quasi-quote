;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/xml :in test))

(def test test/xml/1 ()
  (is (string= "<element/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element>})))

(def test test/xml/2 ()
  (is (string= "<element attribute=\"1\"/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element (:attribute 1)>})))

(def test test/xml/3 ()
  (is (string= "<element attribute1=\"1\" attribute2=\"2\"/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element (:attribute1 "1" :attribute2 "2")> })))

(def test test/xml/4 ()
  (is (string= "<element><child/></element>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element ()
                  <child>>})))

(def test test/xml/5 ()
  (is (string= "<element/><element/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element><element>})))

(def test test/xml/6 ()
  (is (string= "<element attribute=\"1\"/>"
               {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element ,(list (make-instance 'xml-attribute :name "attribute" :value "1")) >})))
