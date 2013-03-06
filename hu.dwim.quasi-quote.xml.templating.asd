;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.quasi-quote.xml.templating
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.quasi-quote.xml+cxml
               :hu.dwim.util.temporary-files)
  :components ((:module "source"
                :components ((:module "xml"
                                      :components ((:file "templating")))))))
