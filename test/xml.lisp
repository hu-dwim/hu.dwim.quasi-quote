;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/xml :in test))

(def string=-test test/xml/simple ()
  ("<element/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
     <element>})
  ("<element attribute=\"1\"/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
     <element (:attribute 1)>})
  ("<element attribute1=\"1\" attribute2=\"2\"/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
     <element (:attribute1 "1" :attribute2 "2")> })
  ("<element><child/></element>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
     <element ()
       <child>>})
  ("<element/><element/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
     <element>
     <element>}))

(def string=-test test/xml/attribute-unquoting ()
  ("<element attribute=\"1\"/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
    <element ,(list (make-xml-attribute "attribute" "1"))>})
  (with-expected-failures
    ("<element attribute1=\"1\" attribute2=\"2\" attribute3=\"3\" attribute4=\"4\" aTTriUte5=\"5\" attribute6=\"6\"/>"
     {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
      <element (attribute1 1
                ,(make-xml-attribute "attribute2" "2")
                ,@(list (make-xml-attribute "attribute3" "3")
                        (make-xml-attribute "attribute4" "4"))
                "aTTriUte5" "5"
                ,(make-xml-attribute "attribute6" "6"))>})))
