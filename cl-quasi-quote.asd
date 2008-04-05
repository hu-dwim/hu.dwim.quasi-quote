;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar))

(defpackage #:cl-quasi-quote-system
  (:use :cl :asdf :cl-syntax-sugar)

  (:export #:*load-as-production-p*))

(in-package #:cl-quasi-quote-system)

(defvar *load-as-production-p* t)

(defsystem :cl-quasi-quote
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Quasi quote transformations"
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-quasi-quote::setup-readtable"
  :depends-on (:metabang-bind
               :alexandria
               :iterate
               :defclass-star
               :closer-mop
               :cl-def
               :cl-syntax-sugar
               :babel
               :flexi-streams)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "duplicates" :depends-on ("package"))
             (:file "configuration" :depends-on ("duplicates"))
             (:file "syntax" :depends-on ("configuration"))
             (:file "lisp" :depends-on ("syntax"))
             (:file "bivalent" :depends-on ("binary"))
             (:file "binary" :depends-on ("syntax"))
             (:file "string" :depends-on ("syntax" "binary"))
             (:file "escaping" :depends-on ("syntax"))
             (:file "xml" :depends-on ("string" "escaping"))
             (:file "typesetting" :depends-on ("xml"))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-quasi-quote))))
  (operate 'load-op :cl-quasi-quote-test)
  (in-package :cl-quasi-quote-test)
  (declaim (optimize (debug 3)))
  (pushnew :debug *features*)
  (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-quasi-quote))))
  nil)
