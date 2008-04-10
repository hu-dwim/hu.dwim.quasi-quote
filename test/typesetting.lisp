;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-typesetting-syntax)

(defsuite* (test/typesetting :in test))

(def test-definer typesetting)

(def special-variable *typesetting-stream*)

(def function test-typesetting-ast (expected ast)
  ;; evaluate to string
  (is (string= expected
               (transform-and-emit '(quasi-quoted-xml
                                     quasi-quoted-string
                                     quasi-quoted-string
                                     string-emitting-form
                                     lambda-form
                                     lambda)
                                   ast)))
  ;; write to string stream
  (is (string= expected
               (with-output-to-string (*typesetting-stream*)
                 (transform-and-emit '(quasi-quoted-xml
                                       quasi-quoted-string
                                       quasi-quoted-string
                                       (string-emitting-form :stream-name *typesetting-stream*)
                                       lambda-form
                                       lambda)
                                     ast))))
  ;; evaluate to binary
  (is (string= expected
               (babel:octets-to-string
                (transform-and-emit '(quasi-quoted-xml
                                      quasi-quoted-string
                                      quasi-quoted-binary
                                      binary-emitting-form
                                      lambda-form
                                      lambda)
                                    ast))))
  ;; write to binary stream
  (is (string= expected
               (bind ((*typesetting-stream* (make-in-memory-output-stream)))
                 (transform-and-emit '(quasi-quoted-xml
                                       quasi-quoted-string
                                       quasi-quoted-binary
                                       (binary-emitting-form :stream-name *typesetting-stream*)
                                       lambda-form
                                       lambda)
                                     ast)
                 (babel:octets-to-string (get-output-stream-sequence *typesetting-stream*))))))

(def typesetting-test test/typesetting/simple ()
  ("<table/>"
   [vertical-list])

  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    (text "Hello")])

  ("<table><tr><td><span>Hello</span></td></tr><tr><td><span>World</span></td></tr></table>"
   [vertical-list
    (text "Hello")
    (text "World")]))

(def typesetting-test test/typesetting/mixed ()
  ("<table><tr><td>Hello</td></tr></table>"
   [vertical-list
    {with-quasi-quoted-string-syntax ["Hello"]}])

  ("<table><tr><td><span/></td></tr></table>"
   [vertical-list
    {with-quasi-quoted-xml-syntax <span>}]))

(def typesetting-test test/typesetting/unqoute ()
  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    ,[text "Hello"]])

  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    ,(make-instance 'typesetting-text :contents (list "Hello"))]))
