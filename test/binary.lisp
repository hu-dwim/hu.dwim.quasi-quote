;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/binary :in test))

(defun set-ub8-reader-in-readtable (&optional (readtable *readtable*))
  "Enable a special reader that will read #(1 2 3) into an ub8-vector."
  (bind ((original (get-dispatch-macro-character #\# #\( readtable)))
    (set-dispatch-macro-character #\# #\(
                                  (lambda (stream char1 char2)
                                    (coerce (funcall original stream char1 char2) 'ub8-vector))
                                  readtable)))

(def special-variable *test-binary-stream*)

(def syntax-test-definer binary-test
  (:test-function evaluate-and-compare-binary-emitting-forms
   :readtable-setup (progn
                      (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream* :with-inline-emitting #f)
                      (set-ub8-reader-in-readtable))))

(def syntax-test-definer binary-test/inline
  (:test-function   evaluate-and-compare-binary-emitting-forms
   :readtable-setup (progn
                      (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream* :with-inline-emitting #f)
                      (set-ub8-reader-in-readtable)))
  (:test-function   evaluate-and-compare-binary-emitting-forms
   :readtable-setup (progn
                      (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream* :with-inline-emitting #t)
                      (set-ub8-reader-in-readtable))))

(def function read-from-string-with-binary-syntax (string)
  (with-local-readtable
    (set-ub8-reader-in-readtable)
    (enable-quasi-quoted-binary-syntax :transformation-pipeline *pipeline-to-binary-emitting-form-with-inline-emitting*)
    (read-from-string string)))

(def function pprint-binary (string)
  (pprint (macroexpand (read-from-string-with-binary-syntax string))))

(def function evaluate-and-compare-binary-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*test-binary-stream*)
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (funcall (compile nil lambda-form))))))

(def binary-test test/binary/simple ()
  (#(1 2)
    "`bin(1 2)")
  (#(1 2 3 4)
    "`bin((1 2)
          (3 4))")
  (#(1 2 3 4 5 6 7 8)
    "`bin((1 2)
          ((3 4)
           (5 6))
          (7 8))"))

(def binary-test test/binary/unquote ()
  (#(1 2 3 4 5 6)
    "`bin((1 2)
          ,#(3 4)
          (5 6))")

  (#(1 2 3 4 5 6 7 8 9 10 11 12)
    "`bin((1 2)
          ,(list
            #(3 4)
            `bin((5 6) (7 8))
            #(9 10))
          (b c))")

  (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    "`bin(1
          2
          ,(list
            #(3 4)
            `bin((5 6)
                 ,(list #(7 8) #(9 10))
                 (b c))
            #(13 14))
          f
          10)"))

(def binary-test test/binary/spliced-unquote ()
  (#(1 2 3 4 5 6 7)
    "`bin(1
          ,(make-array 1 :initial-element 2)
          ,@(list #(3) #(4) #(5))
          ,(make-array 1 :initial-element 6)
          7)"))

(def binary-test test/binary/reverse ()
  (#(1 2 3 4 5 6 7 8)
   "`bin(1 2
         ,(reverse (list `bin(5 6)
                         `bin(3 4)))
         7 8)"))

(def binary-test/inline test/binary/ordered-unquote ()
  (#(1 2 3 4 5 6)
    "`bin(1 2
          ,`bin(3 4)
          5 6)")

  (#(1 2 3 4 5 6 7 8 9 10 11 12)
    "`bin(1 2
          ,(list
            `bin(3 4)
            `bin(5 6 7 8)
            `bin(9 a))
          b c)")

  (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    "`bin(1 2
          ,(list
            `bin(3 4)
            `bin(5 6
                 ,(list `bin(7 8) `bin(9 a))
                 b c)
            `bin(d e))
          f 10)"))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mixed non-compatibles

(def special-variable *test-binary-stream-2*)

(def function setup-readtable-for-mixed-test (inline1? inline2?)
  (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream*
                                                             :with-inline-emitting inline1?
                                                             :dispatched-quasi-quote-name 'bin1)
  (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream-2*
                                                             :with-inline-emitting inline2?
                                                             :dispatched-quasi-quote-name 'bin2)
  (set-ub8-reader-in-readtable))

(def syntax-test-definer binary-test/mixed
  (:test-function   evaluate-and-compare-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-test #f #f))
  (:test-function   evaluate-and-compare-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-test #t #t))
  (:test-function   evaluate-and-compare-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-test #t #f))
  (:test-function   evaluate-and-compare-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-test #f #t)))

(def function evaluate-and-compare-binary-emitting-forms/mixed (expected ast)
  (bind ((lambda-form `(lambda ()
                         (bind ((result))
                           (push (with-output-to-sequence (*test-binary-stream*)
                                   (push (with-output-to-sequence (*test-binary-stream-2*)
                                           (emit ,ast))
                                         result))
                                 result)
                           (values-list (nreverse result))))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (multiple-value-list (funcall (compile nil lambda-form)))))))

(def binary-test/mixed test/binary/mixed ()
  (with-expected-failures
    ((list #(1 2 3 4 5 6)
           #(7 8 9 10))
     "`bin1(1 2
           `bin2(7 8 ,(list `bin2(9 a)))
           ,(list `bin1(3 4))
           5 6)")))
