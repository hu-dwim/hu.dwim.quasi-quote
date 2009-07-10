;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote))

(in-package #:cl-quasi-quote-system)

(define-qq-subsystem :cl-quasi-quote-css
  :version "1.0"
  :description "Quasi quote transformations for emitting CSS"
  :setup-readtable-function "cl-quasi-quote-css::setup-readtable"
  :components
  ((:module "src"
            :components
            ((:module "css"
                      :components
                      ((:file "package")
                       (:file "ast" :depends-on ("package"))
                       (:file "syntax" :depends-on ("package" "ast"))
                       (:file "transform" :depends-on ("package" "syntax" "ast"))))))))
