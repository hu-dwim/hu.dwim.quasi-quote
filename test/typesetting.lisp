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
  (bind ((transformed
          (chain-transform '(quasi-quoted-xml
                             quasi-quoted-string
                             quasi-quoted-string
                             string-emitting-form
                             lambda-form
                             lambda)
                           ast)))
    (is (string= expected
                 (emit (funcall transformed)))))
  ;; write to string stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-xml
                             quasi-quoted-string
                             quasi-quoted-string
                             (string-emitting-form :stream *typesetting-stream*)
                             lambda-form
                             lambda)
                           ast)))
    (is (string= expected
                 (bind ((*typesetting-stream* (make-string-output-stream)))
                   (emit (funcall transformed) *typesetting-stream*)
                   (get-output-stream-string *typesetting-stream*)))))
  ;; evaluate to binary
  (bind ((transformed
          (chain-transform '(quasi-quoted-xml
                             quasi-quoted-string
                             quasi-quoted-binary
                             binary-emitting-form
                             lambda-form
                             lambda)
                           ast)))
    (is (string= expected
                 (babel:octets-to-string (emit (funcall transformed))))))
  ;; write to binary stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-xml
                             quasi-quoted-string
                             quasi-quoted-binary
                             (binary-emitting-form :stream *typesetting-stream*)
                             lambda-form
                             lambda)
                           ast)))
    (is (string= expected
                 (bind ((*typesetting-stream* (flexi-streams:make-in-memory-output-stream)))
                   (emit (funcall transformed) *typesetting-stream*)
                   (babel:octets-to-string (flexi-streams:get-output-stream-sequence *typesetting-stream*)))))))

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
