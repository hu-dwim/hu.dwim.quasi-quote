;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Quasi quoted domain specific languages and transformations"
  :depends-on (:babel
               :babel-streams
               :hu.dwim.common-lisp
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "configuration" :depends-on ("duplicates"))
                             (:module "generic"
                              :depends-on ("configuration")
                              :components ((:file "util")
                                           (:file "syntax" :depends-on ("util"))
                                           (:file "transformation" :depends-on ("util" "syntax"))
                                           (:file "list" :depends-on ("syntax" "transformation" "util"))
                                           (:file "bivalent" :depends-on ("transformation" "syntax" "string" "binary" "util"))
                                           (:file "binary" :depends-on ("transformation" "syntax" "util"))
                                           (:file "string" :depends-on ("transformation" "syntax" "binary" "util"))))))))
