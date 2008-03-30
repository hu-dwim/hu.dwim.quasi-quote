;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-test
  (:nicknames :qqt)

  (:use :common-lisp
        :metabang-bind
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-quasi-quote))

(import
 '(escape-as-xml)
 (find-package :cl-quasi-quote-test))

(in-package :cl-quasi-quote-test)

(in-root-suite)

(defsuite* test)

(def definer string=-test (name args &body body)
  (labels ((process-entry (entry)
             (if (eq (first entry) 'with-expected-failures)
                 `(with-expected-failures
                    ,@(mapcar #'process-entry (rest entry)))
                 (bind (((expected form) entry))
                   `(is (string= ,expected ,form))))))
    `(def test ,name ,args
       ,@(mapcar #'process-entry body))))

(def definer binary=-test (name args &body body)
  (labels ((process-entry (entry)
             (if (eq (first entry) 'with-expected-failures)
                 `(with-expected-failures
                    ,@(mapcar #'process-entry (rest entry)))
                 (bind (((expected form) entry))
                   `(is (equalp ,expected ,form))))))
    `(def test ,name ,args
       ,@(mapcar #'process-entry body))))
