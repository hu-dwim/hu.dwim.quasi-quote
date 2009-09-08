;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.css
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Quasi quote transformations for emitting CSS"
  :depends-on (:hu.dwim.quasi-quote)
  :components ((:module "source"
                :components ((:module "css"
                              :components ((:file "package")
                                           (:file "ast" :depends-on ("package"))
                                           (:file "syntax" :depends-on ("package" "ast"))
                                           (:file "transform" :depends-on ("package" "syntax" "ast"))))))))
