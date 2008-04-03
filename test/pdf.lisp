;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-pdf-syntax)

(defsuite* (test/pdf :in test))

(def test-definer pdf)

(def special-variable *pdf-stream*)

(def function test-pdf-ast (expected ast)
  ;; evaluate to binary
  (bind ((transformed
          (chain-transform '(quasi-quoted-bivalent
                             quasi-quoted-binary
                             binary-emitting-form
                             lambda-form
                             lambda)
                           ast)))
    (is (string= expected
                 (babel:octets-to-string (emit (funcall transformed))))))
  ;; write to binary stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-bivalent
                             quasi-quoted-binary
                             (binary-emitting-form :stream *pdf-stream*)
                             lambda-form
                             lambda)
                           ast)))
    (is (string= expected
                 (bind ((*pdf-stream* (flexi-streams:make-in-memory-output-stream)))
                   (emit (funcall transformed) *pdf-stream*)
                   (babel:octets-to-string (flexi-streams:get-output-stream-sequence *pdf-stream*)))))))
