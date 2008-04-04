;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

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
  [document (info (dictionary (name "Author") (string "levy")))
            (root (catalog (name "Pages")
                           (pages (name "Count") (number 0)
                                  (name "MediaBox") (array (number 0) (number 0) (number 612) (number 792)))))])

(def pdf-test test/pdf/simple ()
  [document (info (dictionary (name "Author") (string "levy")))
            (root (catalog (name "Pages")
                           (pages (name "Count") (number 0)
                                  (name "Kids") (array (page (name "Resources")
                                                             (dictionary (name "ProcSet") (array (name "PDF") (name "Text"))
                                                                         (name "Font") (dictionary (name "F4")
                                                                                                   (dictionary (name "Type") (name "Font")
                                                                                                               (name "Subtype") (name "Type1")
                                                                                                               (name "Name") (name "F4")
                                                                                                               (name "BaseFont") (name "Times-Roman"))))
                                                             (name "Contents")
                                                             (string "Hello World")))
                                  (name "MediaBox") (array (number 0) (number 0) (number 612) (number 792)))))])
