;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/xml :in test))

(def test test/xml/escaping ()
  (is (string= "&lt;1&quot;2&gt;3&lt;&amp;4&gt;"
               (escape-as-xml "<1\"2>3<&4>")))
  (let ((str "alma"))
    (is (eq str (escape-as-xml str)))))

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

(def string=-test test/xml/element-unquoting ()
  ("<element/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
    <,"element">})
  ("<element><nested/></element>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
    <element ()
      ,(make-xml-element "nested")>})
  ("<element><child1/><child2/><child3/><child4 attribute1=\"1\"/><child5/></element>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
    <element ()
      <child1>
      ,(make-xml-element "child2")
      ,@(list (make-xml-element "child3")
              (make-xml-element "child4" (list (make-xml-attribute "attribute1" "1"))))
      <child5>>}))

(def string=-test test/xml/attribute-unquoting ()
  ("<element attribute=\"1\"/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
    <element ,(list (make-xml-attribute "attribute" "1"))>})
  ("<element attribute1=\"1\" attribute2=\"2\" attribute3=\"3\" attribute4=\"4\" aTTriUte5=\"5\" attribute6=\"6\"/>"
   {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'string-emitting-form)
    <element (attribute1 1
                         ,(make-xml-attribute "attribute2" "2")
                         ,@(list (make-xml-attribute "attribute3" "3")
                                 (make-xml-attribute "attribute4" "4"))
                         "aTTriUte5" "5"
                         ,(make-xml-attribute "attribute6" "6"))>}))
