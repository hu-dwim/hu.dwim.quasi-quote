;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.xml+cxml
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.quasi-quote.xml::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :depends-on (:cxml
               :hu.dwim.quasi-quote.xml)
  :components ((:module "integration"
                :components ((:file "xml+cxml")))))
