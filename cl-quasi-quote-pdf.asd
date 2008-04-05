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
                       (:file "pdf" :depends-on ("package"))))))))

(defsystem :cl-quasi-quote-pdf-test
  :description "Tests for cl-quasi-quote-pdf."
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-quasi-quote::setup-readtable"
  :depends-on (:cl-quasi-quote-pdf ;; and everything else it depends on...
               :cl-quasi-quote-test
               :stefil
               )
  :components
  ((:module "test"
	    :components
            ((:file "pdf")))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-quasi-quote-pdf))))
  (operate 'load-op :cl-quasi-quote-pdf-test)
  (in-package :cl-quasi-quote-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test/pdf))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-quasi-quote-pdf))))
  nil)
