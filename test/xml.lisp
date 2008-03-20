;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/xml :in test))

(def test test/xml/1 ()
  (is (string= "<element/>"
               {(with-quasi-quoted-transformed-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element>})))

#|
(def test test/xml/2 ()
  (is (string= "<element attribute=\"1\"/>"
               {(with-quasi-quoted-transformed-syntax 'quasi-quoted-xml 'string-emitting-form)
                <element :attribute 1>})))

(def test test/xml/3 ()
  (is (string= "<element attribute1=\"1\" attribute2=\"2\"/>"
               (transform-quasi-quoted-xml-to-string
                (make-instance 'quasi-quote
                               :body (make-instance 'xml-element
                                                    :name "element"
                                                    :attributes (list
                                                                 (make-instance 'xml-attribute
                                                                                :name "attribute1"
                                                                                :value "1")
                                                                 (make-instance 'xml-attribute
                                                                                :name "attribute2"
                                                                                :value "2"))))))))

(def test test/xml/4 ()
  (is (string= "<element><child/></element>"
               (transform-quasi-quoted-xml-to-string
                (make-instance 'quasi-quote
                               :body (make-instance 'xml-element
                                                    :name "element"
                                                    :children (list
                                                               (make-instance 'xml-element
                                                                              :name "child"))))))))

(def test test/xml/5 ()
  (is (string= "<element/><element/>"
               (transform-quasi-quoted-xml-to-string
                (make-instance 'quasi-quote
                               :body (list
                                      (make-instance 'xml-element
                                                     :name "element")
                                      (make-instance 'xml-element
                                                     :name "element")))))))
|#