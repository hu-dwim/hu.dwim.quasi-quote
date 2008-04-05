;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl-quasi-quote-pdf :cl-quasi-quote-test))

(enable-quasi-quoted-pdf-to-binary-stream-emitting-form-syntax '*pdf-stream*)

(defsuite* (test/pdf :in test))

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
   [info [dictionary [name "Author"] [string "levy"]]]
   [root [catalog [name "Pages"]
                  [pages [name "Count"] [number 0]
                         [name "MediaBox"] [array [number 0] [number 0] [number 612] [number 792]]]]]])

(def pdf-test test/pdf/simple ()
  [document
   [info [dictionary [name "Author"] [string "levy"]]]
   [root [catalog [name "Pages"] [indirect-object-reference pages]]]
   [indirect-object pages
                    [pages [name "Count"] [number 1]
                           [name "Kids"] [array [indirect-object-reference page1]]
                           [name "MediaBox"] [array [number 0] [number 0] [number 612] [number 792]]]]
   [indirect-object page1
                    [page [name "Parent"]
                          [indirect-object-reference pages]
                          [name "Resources"]
                          [dictionary [name "ProcSet"] [array [name "PDF"] [name "Text"]]
                                      [name "Font"] [dictionary [name "F1"]
                                                                [dictionary [name "Type"] [name "Font"]
                                                                            [name "Subtype"] [name "Type1"]
                                                                            [name "Name"] [name "F1"]
                                                                            [name "BaseFont"] [name "Times-Roman"]]]]
                          [name "Contents"] [indirect-object-reference stream1]]]
   [indirect-object stream1
                    [stream [begin-text]
                            [set-font "F1" 12]
                            [move-text 72 712]
                            [string "Hello World"]
                            [display-text]
                            [end-text]]]])
