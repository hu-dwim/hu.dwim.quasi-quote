;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote))

(in-package #:cl-quasi-quote-system)

(defsubsystem :cl-quasi-quote-js
  :version "0.1"
  :description "Quasi quote transformations for emitting JavaScript"
  :setup-readtable-function "cl-quasi-quote-js::setup-readtable"
  :depends-on (:cl-ppcre
               :cl-quasi-quote
               :cl-walker
               )
  :components
  ((:module "src"
            :components
            ((:module "js"
                      :components
                      ((:file "package")
                       (:file "escaping" :depends-on ("package"))
                       (:file "repositories" :depends-on ("package"))
                       (:file "ast" :depends-on ("package" "repositories"))
                       (:file "syntax" :depends-on ("package" "ast" "repositories"))
                       (:file "transform" :depends-on ("package" "escaping" "syntax" "ast"))
                       (:file "js-utils" :depends-on ("transform"))))))))
