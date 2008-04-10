;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

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
  (is (string=-ignoring-whitespaces
       expected
       (transform-and-emit '(quasi-quoted-string
                             string-emitting-form
                             lambda-form
                             lambda)
                           ast)))
  ;; write to string stream
  (is (string=-ignoring-whitespaces
       expected
       (with-output-to-string (*js-stream*)
         (transform-and-emit '(quasi-quoted-string
                               (string-emitting-form :stream-name *js-stream*)
                               lambda-form
                               lambda)
                             ast))))
  ;; evaluate to binary
  (is (string=-ignoring-whitespaces
       expected
       (babel:octets-to-string
        (transform-and-emit '(quasi-quoted-string
                              quasi-quoted-binary
                              binary-emitting-form
                              lambda-form
                              lambda)
                            ast))))
  ;; write to binary stream
  (is (string=-ignoring-whitespaces
       expected
       (bind ((*js-stream* (make-in-memory-output-stream)))
         (transform-and-emit '(quasi-quoted-string
                               quasi-quoted-binary
                               (binary-emitting-form :stream-name *js-stream*)
                               lambda-form
                               lambda)
                             ast)
         (babel:octets-to-string (get-output-stream-sequence *js-stream*))))))

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
