;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.quasi-quote.js
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Quasi quote transformations for emitting JavaScript."
  :depends-on (:cl-ppcre
               :hu.dwim.quasi-quote
               :hu.dwim.util.temporary-files
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
