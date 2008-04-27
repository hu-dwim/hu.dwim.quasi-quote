;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-ui-syntax)

(defsuite* (test/ui :in test))

(def test-definer ui-xml)

(def definer ui-pdf-test (name args &body forms)
  `(def test ,name ,args
     (finishes
       (test-ui-pdf-ast ',name ,@forms))))

(def special-variable *ui-stream*)

(def function test-ui-xml-ast (expected ast)
  ;; evaluate to string
  (is (string= expected
               (transform-and-emit '(quasi-quoted-xml
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

(def function test-ui-pdf-ast (name ast)
  (with-open-file (*pdf-stream* (string-downcase (concatenate 'string "/tmp/" (substitute #\- #\/ (symbol-name name)) ".pdf"))
                                :direction :output :element-type '(unsigned-byte 8) :if-does-not-exist :create :if-exists :supersede)
    (transform-and-emit '(quasi-quoted-ui
                          quasi-quoted-pdf
                          quasi-quoted-bivalent
                          quasi-quoted-binary
                          (binary-emitting-form :stream-name *pdf-stream*)
                          lambda-form
                          lambda)
                        ast)))

(def ui-xml-test test/ui/xml/simple ()
  ("<table/>"
   [vertical-list])

  ("<table><tr><td><span>Hello</span></td></tr></table>"
   [vertical-list
    [text "Hello"]])

  ("<table><tr><td><span>Hello</span></td></tr><tr><td><span>World</span></td></tr></table>"
   [vertical-list
    [text "Hello"]
    [text "World"]]))

(def ui-pdf-test test/ui/pdf/hello-world ()
  [screen
   [text "Hello World"]])

(def ui-pdf-test test/ui/pdf/simple ()
  [screen
   [vertical-list
    [text "Hello World"]
    [table
      [row
        [cell
          [text "one"]]
        [cell
          [text "Two"]]]]]])