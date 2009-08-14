;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.js
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Quasi quote transformations for emitting JavaScript"
  :depends-on (:cl-ppcre
               :hu.dwim.quasi-quote
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:module "js"
                              :components ((:file "package")
                                           (:file "escaping" :depends-on ("package"))
                                           (:file "repositories" :depends-on ("package"))
                                           (:file "ast" :depends-on ("package" "repositories"))
                                           (:file "syntax" :depends-on ("package" "ast" "repositories"))
                                           (:file "transform" :depends-on ("package" "escaping" "syntax" "ast"))
                                           (:file "js-utils" :depends-on ("transform"))))))))
