;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-quasi-quote)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar))

(in-package :cl-quasi-quote-system)

(setf *load-as-production-p* nil)

(defsystem :cl-quasi-quote-test
  :description "Tests for cl-quasi-quote."
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-quasi-quote::setup-readtable"
  :depends-on (:metabang-bind
               :iterate
               :stefil
               :cl-def
               :cl-syntax-sugar
               :cl-quasi-quote)
  :components
  ((:module :test
	    :components
            ((:file "package")
             (:file "binary" :depends-on ("package"))
             (:file "string" :depends-on ("package"))
             (:file "bivalent" :depends-on ("package"))
             (:file "xml" :depends-on ("package"))
             (:file "typesetting" :depends-on ("package"))))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-quasi-quote-test))))
  (in-package :cl-quasi-quote-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features*, set (declaim (optimize (debug 3)))."))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-quasi-quote-test))))
  nil)
