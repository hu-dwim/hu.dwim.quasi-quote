;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/xml :in test))

(defun with-quasi-quoted-xml ()
  (lambda (reader)
    (set-quasi-quote-syntax-in-readtable
     (lambda (body) (make-instance 'quasi-quote
                              :body (if (= 1 (length body))
                                        (make-instance 'xml-element :name (string-downcase (first body)))
                                        body)))
     (lambda (form spliced) (make-instance 'unquote :form form :spliced spliced))
     :quasi-quote-character #\<
     :quasi-quote-end-character #\>)
    (first (funcall reader))))

(def test test/xml/1 ()
  (is (string= "<element/>"
               (transform-quasi-quoted-xml-to-string
                {with-quasi-quoted-xml
                    <element>}))))

(def test test/xml/2 ()
  (is (string= "<element attribute=\"1\"/>"
               (transform
                {with-quasi-quoted-xml
                    <element :attribute 1>}
                '(string (byte-array :utf-8))))))

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
