;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

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
  :setup-readtable-function "cl-quasi-quote-test::setup-readtable"
  :depends-on (:metabang-bind
               :iterate
               :stefil
               :cl-def
               :cl-syntax-sugar
               :cl-quasi-quote
               :parse-number
               :bordeaux-threads
               :trivial-shell
               :cxml
               ;; TODO lags behind in the refactor :cl-quasi-quote-pdf
               :cl-quasi-quote-xml
               :cl-quasi-quote-js
               :cl-quasi-quote-css
               :swank
               )
  :components
  ((:module :test
	    :components
            ((:file "package")
             (:file "suite" :depends-on ("package"))
             (:file "list" :depends-on ("suite"))
             (:file "binary" :depends-on ("suite"))
             (:file "string" :depends-on ("suite"))
             (:file "bivalent" :depends-on ("suite"))
             (:file "xml" :depends-on ("suite" "string"))
             (:file "js" :depends-on ("suite" "string" "xml"))
             (:file "css" :depends-on ("suite"))
             ;;(:file "pdf" :depends-on ("suite"))
             ))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-quasi-quote-test))))
  (in-package :cl-quasi-quote-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing"))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-quasi-quote-test))))
  nil)
