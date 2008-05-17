;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-js-syntax)

;; this would hang sbcl, see the .asd for details...
#+nil
(unless (search "JavaScript" (nth-value 1 (trivial-shell:shell-command "js --version")))
  (error "You need a command line JavaScript interpreter for the cl-quasi-quote-js tests. Install the spidermonkey-bin package for one..."))

(def (function d) eval-js (string)
  (bind (((:values stdout nil return-code) (trivial-shell:shell-command "js" :input string)))
    (is (= 0 return-code))
    (with-input-from-string (input stdout)
      (bind ((result (read-line input #f)))
        ;; KLUDGE parse-number::invalid-number is not a serious-condition...
        (or (ignore-some-conditions (parse-number::invalid-number serious-condition)
              (parse-number:parse-number result))
            result)))))

(def function read-from-string-with-js-syntax (string)
  (with-local-readtable
    (enable-quasi-quoted-js-to-string-emitting-form-syntax)
    (read-from-string string)))

(def function pprint-js (string &key (indent 2))
  (bind ((*js-indent* indent))
    (pprint (macroexpand (read-from-string-with-js-syntax string)))))

(defsuite* (test/js :in test) ()
  ;; TODO it's just a proof of concept for now...
  (with-expected-failures
    (run-child-tests)))

(def syntax-test-definer js quasi-quoted-js)

(def special-variable *js-stream*)

(def function whitespace? (char)
  (member char '(#\Space #\Newline) :test #'char=))

(def function string=-ignoring-whitespaces (a b)
  (string= (remove-if #'whitespace? a)
           (remove-if #'whitespace? b)))

;; TODO this is almost the same as test-xml-ast
(def (function d) test-js-ast (expected ast)
  ;; evaluate to string
  (flet ((equal~ (a b)
           (if (and (typep a 'float)
                    (typep b 'float))
               (< (abs (- a b)) 0.0000001)
               (equal a b))))
    (is (equal~ expected
                (eval-js
                 (transform-and-emit '(quasi-quoted-string
                                       string-emitting-form
                                       lambda-form
                                       lambda)
                                     ast))))
    ;; write to string stream
    (is (equal~ expected
                (eval-js
                 (with-output-to-string (*js-stream*)
                   (transform-and-emit '(quasi-quoted-string
                                         (string-emitting-form :stream-name *js-stream*)
                                         lambda-form
                                         lambda)
                                       ast)))))
    ;; evaluate to binary
    (is (equal~ expected
                (eval-js
                 (babel:octets-to-string
                  (transform-and-emit '(quasi-quoted-string
                                        quasi-quoted-binary
                                        binary-emitting-form
                                        lambda-form
                                        lambda)
                                      ast)))))
    ;; write to binary stream
    (is (equal~ expected
                (eval-js
                 (with-output-to-sequence (*js-stream* :return-as 'string)
                   (transform-and-emit '(quasi-quoted-string
                                         quasi-quoted-binary
                                         (binary-emitting-form :stream-name *js-stream*)
                                         lambda-form
                                         lambda)
                                       ast)))))))

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
         (print (setf x (+ 2 (,'alma) x 5))))｣)
  (42
   ｢`js(progn
         ;; inject text without being transformed
         ,(make-string-quasi-quote "a = 22")
         (print (+ ,(make-string-quasi-quote "a") 10 ,10)))｣))

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
         (print (setf x (+ 2 (side-effect) x 5))))｣))

(def test test/js/escaping ()
  (let ((str "alma"))
    ;; return the input if there's no need for escaping
    (is (eq str (escape-as-js-string str)))))

;; leave it at the end, because it screws up emacs coloring
(def js-test test/js/string-quoting ()
  ("alma"
   ｢`js (print "alma")｣)
  (｢al'm"a｣
   ｢`js (print "al'm\"a")｣))
