;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.quasi-quote.xml
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Quasi quote transformations for emitting XML."
  :depends-on (:hu.dwim.quasi-quote)
  :components ((:module "source"
                :components ((:module "xml"
                              :components ((:file "package")
                                           (:file "ast" :depends-on ("package"))
                                           (:file "syntax" :depends-on ("package" "ast"))
                                           (:file "escaping" :depends-on ("package"))
                                           (:file "transform" :depends-on ("package" "escaping" "syntax" "ast"))))))))
