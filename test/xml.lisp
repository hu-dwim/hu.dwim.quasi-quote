;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-xml-to-string-emitting-form-syntax)

(defsuite* (test/xml :in test))

(def test test/xml/escaping ()
  (is (string= "&lt;1&quot;2&gt;3&lt;&amp;4&gt;"
               (escape-as-xml "<1\"2>3<&4>")))
  (let ((str "alma"))
    (is (eq str (escape-as-xml str)))))

(def string=-test test/xml/escaping-element-value ()
  ("<element attribute=\"&lt;1&gt;\"/>"
     <element ,(list (make-xml-attribute "attribute" "<1>"))>)
  ("<element attribute=\"&lt;1&gt;\"/>"
     <element (attribute "<1>")>))

(def string=-test test/xml/simple ()
  ("<element/>"
     <element>)
  ("<element attribute=\"1\"/>"
     <element (:attribute 1)>)
  ("<element attribute1=\"1\" attribute2=\"2\"/>"
     <element (:attribute1 "1" :attribute2 "2")> )
  ("<element>Hello</element>"
     <element "Hello">)
  ("<element><child/></element>"
     <element
       <child>>)
  ("<element><child/></element>"
     ;; test () being optional
     <element ()
       <child>>))

(def string=-test test/xml/element-unquoting ()
  ("<element/>"
    <,"element">)
  ("<element><nested/></element>"
    <element
      ,(make-xml-element "nested")>)
  ("<element><child1/><child2/><child3/><child4 attribute1=\"1\"/><child5/></element>"
    <element
      <child1>
      ,(make-xml-element "child2")
      ,@(list (make-xml-element "child3")
              (make-xml-element "child4" (list (make-xml-attribute "attribute1" "1"))))
      <child5>>))

(def string=-test test/xml/attribute-unquoting ()
  ("<element attribute=\"1\"/>"
    <element ,(list (make-xml-attribute "attribute" "1"))>)
  ("<element attribute1=\"1\" attribute2=\"2\" attribute3=\"3\" attribute4=\"4\" aTTriUte5=\"5\" attribute6=\"6\"/>"
    <element (attribute1 1
              ,(make-xml-attribute "attribute2" "2")
              ,@(list (make-xml-attribute "attribute3" "3")
                      (make-xml-attribute "attribute4" "4"))
              "aTTriUte5" "5"
              ,(make-xml-attribute "attribute6" "6"))>))


(def string=-test test/xml/nested-unquoting ()
  ("<a><b><c><d/></c></b></a>"
   <a ,(make-instance 'xml-element
                      :name "b"
                      :children (list <c ,(make-instance 'xml-element :name "d")>))>))
