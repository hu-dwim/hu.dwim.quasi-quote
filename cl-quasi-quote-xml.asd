;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote))

(in-package #:cl-quasi-quote-system)

(define-qq-subsystem :cl-quasi-quote-xml
  :version "0.1"
  :description "Quasi quote transformations for emitting XML"
  :setup-readtable-function "cl-quasi-quote-xml::setup-readtable"
  :components
  ((:module "src"
            :components
            ((:module "xml"
                      :components
                      ((:file "package")
                       (:file "ast" :depends-on ("package"))
                       (:file "syntax" :depends-on ("package" "ast"))
                       (:file "escaping" :depends-on ("package"))
                       (:file "transform" :depends-on ("package" "escaping" "syntax" "ast"))))))))

(define-qq-system-connection :cl-quasi-quote-xml-and-cxml
  :requires (:cl-quasi-quote-xml :cxml)
  :setup-readtable-function "cl-quasi-quote::setup-readtable"
  :components
  ((:module "src"
            :components
            ((:module "xml"
                      :components
                      ((:file "cxml-integration")))))))
