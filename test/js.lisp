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
          #+nil
          (switch (result :test #'string=)
            ("true" (return #t))
            ("false" (return #f))
            ("undefined" (return 'undefined)))
          result)))))

(def special-variable *js-stream*)
(def special-variable *xml/js-stream*)

(def function setup-readtable-for-js-test (&key with-inline-emitting (indentation-width 2) (binary? #f)
                                                (output-prefix #.(format nil "~%<script>~%// <![CDATA[~%"))
                                                (output-postfix #.(format nil "~%// ]]>~%</script>~%"))
                                                (xml? #f) (output-stream-name (if xml? '*xml/js-stream* '*js-stream*)))
  (if binary?
      (progn
        (enable-quasi-quoted-js-syntax
         :transformation-pipeline (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                                   output-stream-name :binary #t :with-inline-emitting with-inline-emitting
                                   :indentation-width indentation-width)
         :nested-transformation-pipeline (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                                          output-stream-name :binary #t :with-inline-emitting with-inline-emitting
                                          :indentation-width indentation-width
                                          :output-prefix output-prefix
                                          :output-postfix output-postfix))
        (when xml?
          (enable-quasi-quoted-xml-to-binary-emitting-form-syntax
           output-stream-name
           :encoding :utf-8
           :indentation-width indentation-width
           :with-inline-emitting with-inline-emitting))
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         output-stream-name
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting))
      (progn
        (enable-quasi-quoted-js-syntax
         :transformation-pipeline (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                                   output-stream-name
                                   :binary #f
                                   :with-inline-emitting with-inline-emitting
                                   :indentation-width indentation-width)
         :nested-transformation-pipeline (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                                          output-stream-name
                                          :binary #f
                                          :with-inline-emitting with-inline-emitting
                                          :indentation-width indentation-width
                                          :output-prefix output-prefix
                                          :output-postfix output-postfix))
        (when xml?
          (enable-quasi-quoted-xml-to-string-emitting-form-syntax
           output-stream-name
           :indentation-width indentation-width
           :with-inline-emitting with-inline-emitting))
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         output-stream-name
         :with-inline-emitting with-inline-emitting))))

(def syntax-test-definer js-test
  (:test-function   test-js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f))
  (:test-function   test-js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f :binary? #t))
  (:test-function   test-js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t))
  (:test-function   test-js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t :binary? #t)))

(def syntax-test-definer xml/js-test
  (:test-function   test-xml/js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f :xml? #t))
  (:test-function   test-xml/js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f :xml? #t :binary? #t))
  (:test-function   test-xml/js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t :xml? #t))
  (:test-function   test-xml/js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t :xml? #t :binary? #t)))

(def function read-from-string-with-xml/js-syntax (string &optional (with-inline-emitting #f) (binary? #f))
  (with-local-readtable
    (setup-readtable-for-js-test :with-inline-emitting with-inline-emitting :xml? #t :binary? binary?)
    (read-from-string string)))

(def function pprint-xml/js (string &optional (with-inline-emitting #f) (binary? #f))
  (pprint (macroexpand (read-from-string-with-xml/js-syntax string with-inline-emitting binary?))))

(def function emit-xml/js (string &optional (with-inline-emitting #f) (binary? #f))
  (with-output-to-string (*xml/js-stream*)
    (emit
     (eval
      (read-from-string-with-xml/js-syntax string with-inline-emitting binary?)))))

(def function js-result-equal (a b)
  (if (and (typep a 'float)
           (typep b 'float))
      (< (abs (- a b)) 0.0000001)
      (equal a b)))

(def (function d) test-js-emitting-forms (expected ast)
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

(def function test-xml/js-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-string (*xml/js-stream*)
                           (emit ,ast))))
         (actual (funcall (compile nil lambda-form))))
    ;;(print (macroexpand-all lambda-form))
    (is (string= expected actual))))

(def function test-xml/js-emitting-forms/binary (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*xml/js-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (string= expected
                 (octets-to-string (funcall (compile nil lambda-form))
                                   :encoding :utf-8)))))

;;;;;;;;;;;;;;;;;;;;;
;;; the tests finally

(def js-test test/js/simple ()
  (42
   ｢`js(print (+ 40 2))｣)
  (42
   ｢`js(let ((a 42))
         (decf a)
         (print (incf a)))｣))

(def js-test test/js/literals ()
  (1.42
   ｢`js(print 1.42)｣)
  ("null"
   ｢`js(print nil)｣)
  ("undefined"
   ｢`js(print undefined)｣)
  ("true"
   ｢`js(print t)｣)
  ("true"
   ｢`js(print true)｣)
  ("false"
   ｢`js(print false)｣)
  ((coerce 1/3 'float)
   ｢`js(print 1/3)｣))

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
  ("true"
   ｢`js(let ((x "-"))
         (.to-string (+ "" x))
         (print (not (not (.match (+ "foo" x "bar") "o-b")))))｣))

(def js-test test/js/if ()
  ("then"
   ｢`js(if (< 2 3)
           (print "then")
           (print "else"))｣)
  ("else"
   ｢`js(if (< 3 2)
           (print "then")
           (print "else"))｣)
  ("else"
   ｢`js(if (< 3 2)
           (progn
             (print "multiple statements")
             (print "then"))
           (progn
             (progn
               (let ((output "else"))
                 (print output)))))｣)
  ("ok"
   ｢`js(if (< 3 2)
           (if (< 2 3)
               (print "then")
               (print "else"))
           (print "ok"))｣))

(def js-test test/js/if-as-expression ()
  ("else"
   ｢`js(let ((x (if (< 3 2)
                    "then"
                    "else")))
         (print x))｣)
  ("then"
   ｢`js(print (if (< 2 3)
                  "then"
                  "else"))｣))

(def js-test test/js/cond ()
  ("third"
   ｢`js(cond ((< 3 2)
              (print "first"))
             ((< 5 4)
              (print "second"))
             ((< 1 2)
              (print "third"))
             (t (print "default")))｣)
  ("second"
   ｢`js(cond ((< 3 2)
              (print "more statements")
              (print "first"))
             ((< 4 5)
              (print "second"))
             (t (print "default")))｣)
  ("first"
   ｢`js(cond ((< 2 3)
              (print "first"))
             (t (print "default")))｣)
  ("no default"
   ｢`js(cond ((< 2 3)
              (print "no default")))｣))

(def js-test test/js/conditionals ()
  ("ok"
   ｢`js(if (and (or (not true) true)
                (not false))
           (print "ok")
           (print "wrong"))｣))

(def js-test test/js/do ()
  (15
   ｢`js(let* ((vector #(1 2 3 4 5))
              (length (slot-value vector 'length))
              (sum 0))
         (do ((idx 0 (1+ idx)))
             ((>= idx length))
           (incf sum (aref vector idx)))
         (print sum))｣))

(def js-test test/js/unwind-protect ()
  (45
   ｢`js(let ((a 42))
         (unwind-protect
              (progn
                (incf a)
                (incf a))
           (incf a))
         (print a))｣))

(def js-test test/js/try-catch ()
  (54
   ｢`js(let ((a 42))
         (try
              (progn
                (incf a)
                (throw 10)
                (setf a 0))
           (catch (e)
             (incf a e))
           (finally
            (incf a)))
         (print a))｣))

(def js-test test/js/arrays ()
  ("10,20"
   ｢`js(print (.to-string (vector 10 20)))｣)
  (10
   ｢`js(print (aref (list 10 20) 0))｣)
  (20
   ｢`js(print (elt (vector 10 20) 1))｣))

(def test test/js/array-errors ()
  (flet ((transform (string)
           (transform (macroexpand (read-from-string-with-xml/js-syntax string)))))
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

(def js-test test/js/macolet ()
  (42
   ｢`js(macrolet ((macro (var value &body body)
                    `(let ((,var ,value))
                       ,@body)))
         (macro a 42 (print a)))｣))

(def js-test test/js/defun ()
  (with-expected-failures
    (3
     ｢`js(progn
           (defun x (a &key (b 42) &allow-other-keys)
             (return (+ a b)))
           (print (x 1 :b 2)))｣)))

(def js-test test/js/lambda ()
  ("foobarbaz"
   ｢`js(print
        ((lambda (fn x)
           (return (fn (+ x "bar"))))
         (lambda (x)
           (return (+ x "baz")))
         "foo"))｣))

(def test test/js/escaping ()
  (let ((str "alma"))
    ;; return the input if there's no need for escaping
    (is (eq str (escape-as-js-string str)))))

(def js-test test/js/mixed-with-string-quasi-quote ()
  (42
   ｢`js(progn
         ;; this way you can inject untransformed (not even escaped) text into the js output
         `str("a = 22")
         (print (+ `str("a") 10 ,10)))｣))

(def test test/js/mixed-with-xml ()
  (bind ((emitted (emit-xml/js ｢<body `js(+ 2 2)>｣))
         (body (parse-xml-into-sxml emitted)))
    (is (string= (first body) "body"))
    (bind ((script (third body)))
      (is (string= (first script) "script"))
      (is (stringp (third script)))
      (is (search "2 + 2" (third script))))))

;; leave it at the end, because it screws up emacs coloring
(def js-test test/js/string-quoting ()
  ("alma"
   ｢`js(print "alma")｣)
  (｢al'm"a｣
   ｢`js(print "al'm\"a")｣))

#|
REPL demos

(bind ((code-as-string
        ｢<body
         ,(concatenate 'string "some runtime" " generated <escaped> text")
         `str("***<put some unescaped text here!>***")
         ;; let's insert some JavaScript here, with some unquoted runtime part:
         `js(print (+ 2 ,(+ 20 20))
             `str(#\Newline "***<put one more unescaped text here!>***"))>｣))
  (pprint-xml/js code-as-string t)
  (emit-xml/js code-as-string))

|#
