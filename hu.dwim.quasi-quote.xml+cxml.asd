;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.quasi-quote.xml+cxml
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:cxml
               :hu.dwim.quasi-quote.xml)
  :components ((:module "integration"
                :components ((:file "xml+cxml")))))
