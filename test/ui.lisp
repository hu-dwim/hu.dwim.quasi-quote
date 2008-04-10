;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test-ui)

(enable-quasi-quoted-ui-syntax)

(defsuite* (test/ui :in test))

(def test-definer ui)

(def special-variable *ui-stream*)

(def function test-ui-ast (expected ast)
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
               (with-output-to-string (*ui-stream*)
                 (transform-and-emit '(quasi-quoted-xml
                                       quasi-quoted-string
                                       quasi-quoted-string
                                       (string-emitting-form :stream-name *ui-stream*)
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
               (bind ((*ui-stream* (make-in-memory-output-stream)))
                 (transform-and-emit '(quasi-quoted-xml
                                       quasi-quoted-string
                                       quasi-quoted-binary
                                       (binary-emitting-form :stream-name *ui-stream*)
                                       lambda-form
                                       lambda)
                                     ast)
                 (babel:octets-to-string (get-output-stream-sequence *ui-stream*))))))

(def ui-test test/ui/simple ()
  ("<table/>"
   [vertical-list])

  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    (text "Hello")])

  ("<table><tr><td><span>Hello</span></td></tr><tr><td><span>World</span></td></tr></table>"
   [vertical-list
    (text "Hello")
    (text "World")]))

(def ui-test test/ui/mixed ()
  ("<table><tr><td>Hello</td></tr></table>"
   [vertical-list
    {with-quasi-quoted-string-syntax ["Hello"]}])

  ("<table><tr><td><span/></td></tr></table>"
   [vertical-list
    {with-quasi-quoted-xml-syntax <span>}]))

(def ui-test test/ui/unqoute ()
  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    ,[text "Hello"]])

  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    ,(make-instance 'ui-text :contents (list "Hello"))]))
