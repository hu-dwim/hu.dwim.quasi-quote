;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test-js)

(enable-quasi-quoted-js-syntax)

(defsuite* (test/js :in test))

(def test-definer js)

(def special-variable *js-stream*)

(def function whitespace? (char)
  (member char '(#\Space #\Newline) :test #'char=))

(def function string=-ignoring-whitespaces (a b)
  (string= (remove-if #'whitespace? a)
           (remove-if #'whitespace? b)))

;; TODO this is almost the same as test-xml-ast
(def function test-js-ast (expected ast)
  ;; evaluate to string
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             string-emitting-form
                             lambda-form
                             lambda)
                           ast)))
    (is (string=-ignoring-whitespaces
         expected
         (emit (funcall transformed)))))
  ;; write to string stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             (string-emitting-form :stream *js-stream*)
                             lambda-form
                             lambda)
                           ast)))
    (is (string=-ignoring-whitespaces
         expected
         (with-output-to-string (*js-stream*)
           (emit (funcall transformed) *js-stream*)))))
  ;; evaluate to binary
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             quasi-quoted-binary
                             binary-emitting-form
                             lambda-form
                             lambda)
                           ast)))
    (is (string=-ignoring-whitespaces
         expected
         (babel:octets-to-string (emit (funcall transformed))))))
  ;; write to binary stream
  (bind ((transformed
          (chain-transform '(quasi-quoted-string
                             quasi-quoted-binary
                             (binary-emitting-form :stream *js-stream*)
                             lambda-form
                             lambda)
                           ast)))
    (is (string=-ignoring-whitespaces
         expected
         (bind ((*js-stream* (flexi-streams:make-in-memory-output-stream)))
           (emit (funcall transformed) *js-stream*)
           (babel:octets-to-string (flexi-streams:get-output-stream-sequence *js-stream*)))))))

(def test test/js/escaping ()
  (let ((str "alma"))
    (is (eq str (escape-as-js str)))))

(def js-test test/js/simple ()
  ("a + b;"
   #J(+ a b))
  ("{ !(a--); b++; }"
   #J(progn
       (not (decf a))
       (incf b))))

(def js-test test/js/unquote ()
  ("x + 42"
   (bind ((a 2))
     #J(progn
         (+ x ,(+ 40 a))))))
