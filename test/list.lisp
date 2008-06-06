;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/list :in test))

(def function setup-readtable-for-list-test (&optional (inline? #t))
  (enable-quasi-quoted-list-to-list-emitting-form-syntax :with-inline-emitting inline?))

(def syntax-test-definer list-test
  (:test-function   test-list-emitting-forms
   :readtable-setup (setup-readtable-for-list-test #f))
  (:test-function   test-list-emitting-forms
   :readtable-setup (setup-readtable-for-list-test #t)))

(def function test-list-emitting-forms (expected ast)
  (is (equalp expected (eval ast))))

(def function read-from-string-with-list-syntax (string &optional (with-inline-emitting :as-is))
  (with-local-readtable
    (enable-quasi-quoted-list-to-list-emitting-form-syntax :with-inline-emitting with-inline-emitting)
    (read-from-string string)))

(def function pprint-list (string &optional (with-inline-emitting :as-is))
  (pprint (macroexpand (read-from-string-with-list-syntax string with-inline-emitting))))

(def list-test test/list/simple ()
  (`(a b)
    ｢`(a b)｣)

  (`(a b (1 2))
    ｢`(a b (1 2))｣))

(def list-test test/list/unquote ()
  (`(a b ,(list 1 2))
    ｢`(a b ,(list 1 2))｣)

  (`(a b ,(list 1 `("call" 'me) 3))
    ｢`(a b ,(list 1 `("call" 'me) 3))｣)

  (`(a b ,(list 1 `("call" 'me ,(list "Al")) 3))
    ｢`(a b ,(list 1 `("call" 'me ,(list "Al")) 3))｣))

(def test test/list/nested ()
  (bind ((stage1 (eval `(let ((b 43))
                          ,(read-from-string-with-list-syntax
                            ｢`(eval (let ((a 42))
                                      `(list x ,a ,,b)))｣)))))
    (break "~S" stage1)
    (is (consp stage1))
    (is (eq (first stage1) 'eval))
    (is (length= 2 stage1))
    (bind ((stage2 (eval stage1)))
      (break "~S" stage2)
      (is (consp stage2))
      (is (eq (first stage2) 'list))
      (is (eql (second stage2) 43))
      (is (length= 3 stage1))
      (bind ((stage3 (eval stage2)))
        (is (equal (list 42 43) stage3))))))
