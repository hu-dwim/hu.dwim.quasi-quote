;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test-pdf)

(enable-quasi-quoted-pdf-to-binary-stream-emitting-form-syntax '*pdf-stream*)

(defsuite* (test/pdf :in cl-quasi-quote-test::test))

(def special-variable *pdf-stream*)

(def function test-pdf-ast (name ast)
  (with-open-file (*pdf-stream* (string-downcase (concatenate 'string "/tmp/" (substitute #\- #\/ (symbol-name name)) ".pdf"))
                                :direction :output :element-type '(unsigned-byte 8) :if-does-not-exist :create :if-exists :supersede)
    (emit-pdf ast *pdf-stream*)))

(def definer pdf-test (name args &body forms)
  `(def test ,name ,args
     (finishes
       (test-pdf-ast ',name ,@forms))))

(def pdf-test test/pdf/empty ()
  [document
   [info [dictionary "Author" "levy"]]
   [root [catalog "Pages" [pages "Count" 0
                                 "MediaBox" [array 0 0 612 792]]]]])

(def pdf-test test/pdf/simple ()
  [document
   [info [dictionary "Author" "levy"]]
   [root [catalog "Pages" [indirect-object-reference pages]]]
   [indirect-object pages
                    [pages "Count" 1
                           "Kids" [array [indirect-object-reference page1]]
                           "MediaBox" [array 0 0 612 792]]]
   [indirect-object page1
                    [page "Parent" [indirect-object-reference pages]
                          "Resources" [dictionary
                                       "ProcSet" [array [name "PDF"] [name "Text"]]
                                       "Font" [dictionary
                                               "F1" [dictionary
                                                     "Type"     [name "Font"]
                                                     "Subtype"  [name "Type1"]
                                                     "Name"     [name "F1"]
                                                     "BaseFont" [name "Times-Roman"]]]]
                          "Contents" [indirect-object-reference stream1]]]
   [indirect-object stream1
                    [stream
                     [begin-text]
                     [set-font "F1" 12]
                     [move-text 72 712]
                     [string "Hello World"]
                     [display-text]
                     [end-text]]]])
