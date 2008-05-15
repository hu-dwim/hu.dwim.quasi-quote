;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :asdf-system-connections)
    (try :cl-syntax-sugar)))

(defpackage #:cl-quasi-quote-system
  (:use :cl :asdf :cl-syntax-sugar :asdf-system-connections)

  (:export
   #:*load-as-production-p*
   #:project-relative-pathname
   ))

(in-package #:cl-quasi-quote-system)

(defvar *load-as-production-p* t)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :cl-quasi-quote))))

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
               :babel-streams
               )
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "duplicates" :depends-on ("package"))
             (:file "configuration" :depends-on ("duplicates"))
             (:file "syntax" :depends-on ("configuration"))
             (:file "lisp" :depends-on ("syntax"))
             (:file "bivalent" :depends-on ("string" "binary"))
             (:file "binary" :depends-on ("syntax"))
             (:file "string" :depends-on ("syntax" "binary"))))))

(defsystem-connection cl-quasi-quote-and-swank
  :requires (:cl-quasi-quote :swank)
  :components
  ((:module "src"
            :components ((:file "swank-integration")))))

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

(defmacro defsubsystem (name &key components version author maintainer description
                        setup-readtable-function depends-on)
  `(progn
     (defsystem ,name
       :version ,version
       :author ,(or author
                    '("Attila Lendvai <attila.lendvai@gmail.com>"
                      "Tamás Borbély <tomi.borbely@gmail.com>"
                      "Levente Mészáros <levente.meszaros@gmail.com>"))
       :maintainer ,(or maintainer
                        '("Attila Lendvai <attila.lendvai@gmail.com>"
                          "Tamás Borbély <tomi.borbely@gmail.com>"
                          "Levente Mészáros <levente.meszaros@gmail.com>"))
       :licence "BSD / Public domain"
       :description ,description
       :default-component-class cl-source-file-with-readtable
       :class system-with-readtable
       :setup-readtable-function ,setup-readtable-function
       :depends-on ,(or depends-on
                        '(:cl-quasi-quote ; and everything else it depends on...
                          ))
       :components ,components)

     (defmethod perform ((op test-op) (system (eql (find-system ,name))))
       (operate 'test-op :cl-quasi-quote))

     (defmethod operation-done-p ((op test-op) (system (eql (find-system ,name))))
       nil)))
