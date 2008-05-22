;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(in-root-suite)

(defsuite* test)

(def (definer e) syntax-test-definer (test-definer-name &body setup-forms)
  `(def definer ,test-definer-name (name args &body forms)
     (labels ((process-test-entry (test-function entry)
                (if (eq (first entry) 'with-expected-failures)
                    `(with-expected-failures
                       ,@(mapcar (lambda (form)
                                   (process-test-entry test-function form))
                                 (rest entry)))
                    (bind (((expected ast) entry))
                      `(,test-function ,expected (macroexpand (read-from-string ,ast)))))))
       `(def (test d) ,name ,args
          (setup-readtable)
          ,@(iter (for el :in ',setup-forms)
                  (bind (((&key test-function readtable-setup) el))
                    (collect `(bind ((*readtable* (copy-readtable *readtable*)))
                                ,readtable-setup
                                ,@(mapcar (lambda (form)
                                            (process-test-entry test-function form))
                                          forms)))))))))

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


