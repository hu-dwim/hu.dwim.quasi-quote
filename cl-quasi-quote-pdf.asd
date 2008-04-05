;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote))

(in-package #:cl-quasi-quote-system)

(defsystem :cl-quasi-quote-pdf
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Quasi quote transformations for emitting PDF"
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-quasi-quote-pdf::setup-readtable"
  :depends-on (:cl-quasi-quote ;; and everything else it depends on...
               )
  :components
  ((:module "src"
            :components
            ((:module "pdf"
                      :components
                      ((:file "package")
                       (:file "ast" :depends-on ("package"))
                       (:file "syntax" :depends-on ("package" "ast"))
                       (:file "transform" :depends-on ("package" "syntax" "ast"))))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-quasi-quote-pdf))))
  (operate 'test-op :cl-quasi-quote))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-quasi-quote-pdf))))
  nil)
