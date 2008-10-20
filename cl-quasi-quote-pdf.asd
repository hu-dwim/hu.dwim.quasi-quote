;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote))

(in-package #:cl-quasi-quote-system)

(define-qq-subsystem :cl-quasi-quote-pdf
  :version "0.1"
  :description "Quasi quote transformations for emitting PDF"
  :setup-readtable-function "cl-quasi-quote-pdf::setup-readtable"
  :depends-on (:cl-quasi-quote :cffi)
  :components
  ((:module "src"
            :components
            ((:module "pdf"
                      :components
                      ((:file "package")
                       (:file "zlib")
                       (:file "ast" :depends-on ("package"))
                       (:file "syntax" :depends-on ("package" "ast"))
                       (:file "transform" :depends-on ("package" "syntax" "ast"))))))))

