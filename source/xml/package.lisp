;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(defpackage :hu.dwim.quasi-quote.xml
  (:use :babel
        :babel-streams
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.syntax-sugar))

(in-package :hu.dwim.quasi-quote.xml)

(def (function e) import-external-quasi-quote-symbols-for-extensions/xml (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of hu.dwim.quasi-quote-xml but not to its users."
  (import
   '(children-of
     name-of
     attributes-of
     )
   package))

(import-external-quasi-quote-symbols-for-extensions)

(def function setup-readtable ()
  (hu.dwim.quasi-quote::setup-readtable))
