;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(in-root-suite)

(defsuite* test)

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

