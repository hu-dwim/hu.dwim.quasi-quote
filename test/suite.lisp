;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(in-root-suite)

(defsuite* test)

(def (definer e) syntax-test-definer (name syntax)
  `(def definer ,(format-symbol *package* "~A-TEST" name) (name args &body forms)
     (labels ((process-entry (entry)
                (if (eq (first entry) 'with-expected-failures)
                    `(with-expected-failures
                       ,@(mapcar #'process-entry (rest entry)))
                    (bind (((expected ast) entry))
                      `(,',(format-symbol *package* "TEST-~A-AST" name) ,expected (macroexpand (read-from-string ,ast)))))))
       `(def test ,name ,args
          (setup-readtable)
          (,',(format-symbol *package* "ENABLE-~A-SYNTAX" syntax))
          ,@(mapcar #'process-entry forms)))))

;; TODO delme, convert usages to syntax-test-definer. see xml tests...
(def (definer e) test-definer (name)
  `(def definer ,(format-symbol *package* "~A-TEST" name) (name args &body forms)
     (labels ((process-entry (entry)
                (if (eq (first entry) 'with-expected-failures)
                    `(with-expected-failures
                       ,@(mapcar #'process-entry (rest entry)))
                    (bind (((expected ast) entry))
                      `(,',(format-symbol *package* "TEST-~A-AST" name) ,expected ,ast)))))
       `(def test ,name ,args
          ,@(mapcar #'process-entry forms)))))

(def (function e) transform-and-emit (transformation ast)
  (emit transformation (funcall (chain-transform transformation ast))))

