;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/binary :in test))

(def function with-quasi-quoted-binary ()
  (lambda (reader)
    (set-quasi-quote-syntax-in-readtable
     (lambda (body)
       (make-instance 'quasi-quote
                      :body (if (and (consp body)
                                     (vectorp (first body)))
                                (list (coerce (first body) '(simple-array (unsigned-byte 8) (*))))
                                body)))
     (lambda (form spliced)
       (make-instance 'unquote :form form :spliced spliced))
     :quasi-quote-character #\[
     :quasi-quote-end-character #\])
    (first (funcall reader))))

(def test test/binary/1 ()
  (is (equalp #(1 2 3)
              (transform-quasi-quoted-binary-to-binary
               {with-quasi-quoted-binary
                   [#(1 2 3)]}))))