;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-xml-syntax)

(defsuite* (test/xml :in test))

(def test-definer xml)

(def special-variable *xml-stream*)

(def function test-xml-ast (expected ast)
  ;; evaluate to string
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             quasi-quoted-string
                             string-emitting-form)
                           ast)))
    (is (string= expected
                 (qq::body-of (eval transformed)))))
  ;; write to string stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             quasi-quoted-string
                             (string-emitting-form :stream *xml-stream*))
                           ast)))
    (is (string= expected
                 (bind ((*xml-stream* (make-string-output-stream)))
                   (eval transformed)
                   (get-output-stream-string *xml-stream*)))))
  ;; evaluate to binary
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             quasi-quoted-binary
                             binary-emitting-form)
                           ast)))
    (is (string= expected
                 (babel:octets-to-string (qq::body-of (eval transformed))))))
  ;; write to binary stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             quasi-quoted-binary
                             (binary-emitting-form :stream *xml-stream*))
                           ast)))
    (is (string= expected
                 (bind ((*xml-stream* (flexi-streams:make-in-memory-output-stream)))
                   (eval transformed)
                   (babel:octets-to-string (flexi-streams:get-output-stream-sequence *xml-stream*)))))))

(def test test/xml/escaping ()
  (is (string= "&lt;1&quot;2&gt;3&lt;&amp;4&gt;"
               (escape-as-xml "<1\"2>3<&4>")))
  (let ((str "alma"))
    (is (eq str (escape-as-xml str)))))

(def xml-test test/xml/escaping-element-value ()
  ("<element attribute=\"&lt;1&gt;\"/>"
     <element (,@(list (make-xml-attribute "attribute" "<1>"))) >)
  ("<element attribute=\"&lt;1&gt;\"/>"
     <element (attribute "<1>")>))

(def xml-test test/xml/simple ()
  ("<element/>"
     <element>)
  ("<element attribute=\"1\"/>"
     <element (:attribute 1)>)
  ("<element attribute1=\"1\" attribute2=\"2\"/>"
     <element (:attribute1 "1" :attribute2 "2")> )
  ("<element>Hello</element>"
     <element "Hello">)
  ;; test that attribute list is optional
  ("<element><child/></element>"
     <element
       <child>>)
  ("<element><child/></element>"
     <element ()
       <child>>))

(def xml-test test/xml/element-unquoting ()
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

(def xml-test test/xml/attribute-unquoting ()
  ("<element attribute=\"1\"/>"
    <element (,@(list (make-xml-attribute "attribute" "1"))) >)
  ("<element attribute1=\"1\" attribute2=\"2\" attribute3=\"3\" attribute4=\"4\" aTTriUte5=\"5\" attribute6=\"6\"/>"
    <element (attribute1 1
              ,(make-xml-attribute "attribute2" "2")
              ,@(list (make-xml-attribute "attribute3" "3")
                      (make-xml-attribute "attribute4" "4"))
              "aTTriUte5" "5"
              ,(make-xml-attribute "attribute6" "6"))>))

(def test test/xml/errors ()
  (bind ((*readtable* (copy-readtable)))
    (set-quasi-quoted-xml-syntax-in-readtable)
    (signals reader-error
      (read-from-string "<element < >>"))))

(def test test/xml/less-then-sign-at-toplevel ()
  (bind ((*readtable* (copy-readtable)))
    (set-quasi-quoted-xml-syntax-in-readtable)
    (is (equal '< (read-from-string "<")))
    (is (equal '<= (read-from-string "<=")))
    (is (equal '(< a b) (read-from-string "(< a b)")))))

(def xml-test test/xml/less-then-sign-in-unquote ()
  ("<element ok=\"1\"/>"
    <element (,@(when (< 3 4) (list (make-xml-attribute "ok" "1")))) >))

(def xml-test test/xml/nested-unquoting ()
  ("<a><b><c><d/></c></b></a>"
   <a ,(make-xml-element "b" nil (list <c ,(make-xml-element "d")>))>))

(def xml-test test/xml/mixed ()
  ("<element>Hello</element>"
   <element {with-quasi-quoted-string-syntax ["Hello" ,(list "World")]}>))

(def xml-test test/xml/reverse ()
  ("<element><child2/><child1/></element>"
   <element ,@(let ((c1 <child1>)
                    (c2 <child2>))
                   (list c2 c1))>))
