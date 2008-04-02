;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

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
             (:file "bivalent" :depends-on ("syntax"))
             (:file "binary" :depends-on ("bivalent"))
             (:file "string" :depends-on ("bivalent"))
             (:file "escaping" :depends-on ("syntax"))
             (:file "xml" :depends-on ("syntax" "escaping"))
             (:file "typesetting" :depends-on ("xml"))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-quasi-quote))))
  (operate 'load-op :cl-quasi-quote-test)
  (in-package :cl-quasi-quote-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-quasi-quote))))
  nil)
