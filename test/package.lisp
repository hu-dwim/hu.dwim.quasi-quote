;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.quasi-quote.test
  (:use :babel
        :babel-streams
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.quasi-quote.css
        :hu.dwim.quasi-quote.js
        #+nil :hu.dwim.quasi-quote.pdf
        :hu.dwim.quasi-quote.xml
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.walker)

  (:shadowing-import-from :hu.dwim.quasi-quote #:body-of
                          #:parent-of
                          #:form
                          #:map-ast))

(in-package :hu.dwim.quasi-quote.test)

(import-external-quasi-quote-symbols-for-extensions)
