;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-pdf-to-binary-stream-emitting-form-syntax-in-readtable '*pdf-stream*)

(defsuite* (test/pdf :in test))

(def special-variable *pdf-stream*)

(def definer pdf-test (name args &body forms)
  `(def test ,name ,args
     (with-open-file (*pdf-stream* "/tmp/test.pdf" :direction :output :element-type '(unsigned-byte 8) :if-does-not-exist :create :if-exists :supersede)
       (emit ,@forms *pdf-stream*))))

(def pdf-test test/pdf/empty ()
  [document])