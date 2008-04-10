;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote))

(in-package #:cl-quasi-quote-system)

(defsubsystem :cl-quasi-quote-ui
  :version "0.1"
  :description "Quasi quote transformations for emitting UI"
  :setup-readtable-function "cl-quasi-quote-ui::setup-readtable"
  :depends-on (:cl-quasi-quote-xml :cl-quasi-quote-pdf :cffi)
  :components
  ((:module "src"
            :components
            ((:module "ui"
                      :components
                      ((:file "package")
                       (:file "ast" :depends-on ("package"))
                       (:file "syntax" :depends-on ("package" "ast"))
                       (:file "transform" :depends-on ("package" "syntax" "ast"))))))))
