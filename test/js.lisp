;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/js :in test))

;; this would hang sbcl, see the .asd for details...
#+nil
(unless (search "JavaScript" (nth-value 1 (trivial-shell:shell-command "js --version")))
  (error "You need a command line JavaScript interpreter for the cl-quasi-quote-js tests. Install the spidermonkey-bin package for one..."))

(def (function d) eval-js (string)
  (bind (((:values stdout nil return-code) (trivial-shell:shell-command "js" :input string)))
    (is (= 0 return-code))
    (with-input-from-string (input stdout)
      (bind ((result (read-line input #f)))
        (block nil
          ;; KLUDGE parse-number::invalid-number is not a serious-condition...
          (awhen (ignore-some-conditions (parse-number::invalid-number serious-condition)
                   (parse-number:parse-number result))
            (return it))
          (switch (result :test #'string=)
            ("true" (return #t))
            ("false" (return #f))
            ("undefined" (return #f)))
          result)))))

(def special-variable *js-stream*)

(def function setup-readtable-for-js-test (inline? &key (binary? #f) prefix postfix)
  (if binary?
      (progn
        (enable-quasi-quoted-js-to-binary-emitting-form-syntax
         '*js-stream*
         :encoding :utf-8
         :output-prefix prefix
         :output-postfix postfix
         :with-inline-emitting inline?)
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*js-stream*
         :encoding :utf-8
         :with-inline-emitting inline?))
      (progn
        (enable-quasi-quoted-js-to-string-emitting-form-syntax
         '*js-stream*
         :output-prefix prefix
         :output-postfix postfix
         :with-inline-emitting inline?)
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*js-stream*
         :with-inline-emitting inline?))))

(def syntax-test-definer js-test
  (:test-function   test-js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test #f))
  (:test-function   test-js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test #f :binary? #t))
  (:test-function   test-js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test #t))
  (:test-function   test-js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test #t :binary? #t)))

(def function read-from-string-with-js-syntax (string &optional (with-inline-emitting #f) prefix  postfix)
  (with-local-readtable
    (setup-readtable-for-js-test with-inline-emitting :prefix prefix :postfix postfix)
    (read-from-string string)))

(def function pprint-js (string &optional (with-inline-emitting #f))
  (pprint (macroexpand (read-from-string-with-js-syntax string with-inline-emitting))))

(def function js-result-equal (a b)
  (if (and (typep a 'float)
           (typep b 'float))
      (< (abs (- a b)) 0.0000001)
      (equal a b)))

(def function test-js-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-string (*js-stream*)
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (js-result-equal expected
                         (eval-js
                          (funcall (compile nil lambda-form)))))))

(def function test-js-emitting-forms/binary (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*js-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (js-result-equal expected
                         (eval-js
                          (octets-to-string (funcall (compile nil lambda-form))
                                            :encoding :utf-8))))))

;;;;;;;;;;;;;;;;;;;;;
;;; the tests finally

(def js-test test/js/simple ()
  (42
   ｢`js(print (+ 40 2))｣)
  (42
   ｢`js(let ((a 42))
         (decf a)
         (print (incf a)))｣)
  (1.42
   ｢`js (print 1.42)｣)
  ((coerce 1/3 'float)
   ｢`js (print 1/3)｣))

(def js-test test/js/unquote ()
  (42
   ｢`js(let ((a 20))
         (print (+ a ,(+ 20 2))))｣)
  (42
   ｢`js(let ((a ,(+ 20 2)))
         (print (+ a 10 ,10)))｣)
  (14
   ｢`js(let ((x 10))
         (defun ,'alma ()
           (setf x ,(+ 2 2))
           (return 3))
         (print (setf x (+ 2 (,'alma) x 5))))｣))

(def js-test test/js/mixed-with-string-quasi-quote ()
  (42
   ｢`js(progn
         ;; this way you can inject untransformed (not even escaped) text into the js output
         `str("a = 22")
         (print (+ `str("a") 10 ,10)))｣))

(def js-test test/js/expressions ()
  ("beforexafter"
   ｢`js(let ((x "x"))
         (setf x (+ "before" x "after"))
         (print x))｣)
  (14
   ｢`js(let ((x 10))
         (defun side-effect ()
           (setf x 4)
           (return 3))
         (print (setf x (+ 2 (side-effect) x 5))))｣)
  ("foo"
   ｢`js(print (.to-string ((lambda (x) (return x)) "foo")))｣))

(def js-test test/js/dotted-call ()
  (#t
   ｢`js(let ((x "-"))
         (.to-string (+ "" x))
         (print (not (not (.match (+ "foo" x "bar") "o-b")))))｣))

(def js-test test/js/arrays ()
  ("10,20"
   ｢`js(print (.to-string (vector 10 20)))｣)
  (10
   ｢`js(print (aref (list 10 20) 0))｣)
  (20
   ｢`js(print (elt (vector 10 20) 1))｣))

(def test test/js/array-errors ()
  (flet ((transform (string)
           (transform (macroexpand (read-from-string-with-js-syntax string)))))
    (signals js-compile-error
      (transform ｢`js(elt (vector 10 20) 1 2 3 4 5)｣))
    (signals js-compile-error
      (transform ｢`js(aref (vector 10 20))｣))))

(def js-test test/js/slot-value ()
  (1
   ｢`js(print (slot-value (create :a 1 :b 2) 'a))｣)
  (2
   ｢`js(let ((a "b"))
         (print (slot-value (create :a 1 :b 2) a)))｣))

(def js-test test/js/defun ()
  (3
   ｢`js(progn
         (defun x (a &key (b 42) &allow-other-keys)
           (return (+ a b)))
         (print (x 1 :b 2)))｣))

(def test test/js/escaping ()
  (let ((str "alma"))
    ;; return the input if there's no need for escaping
    (is (eq str (escape-as-js-string str)))))

;; leave it at the end, because it screws up emacs coloring
(def js-test test/js/string-quoting ()
  ("alma"
   ｢`js(print "alma")｣)
  (｢al'm"a｣
   ｢`js(print "al'm\"a")｣))
