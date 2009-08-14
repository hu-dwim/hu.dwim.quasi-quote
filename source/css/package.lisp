;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.quasi-quote.css
  (:use :babel
        :babel-streams
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.syntax-sugar))

(in-package :hu.dwim.quasi-quote.css)

(def (function e) import-external-quasi-quote-symbols-for-extensions/css (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of hu.dwim.quasi-quote-css but not to its users."
  (import
   '(children-of
     name-of
     attributes-of
     )
   package))

(import-external-quasi-quote-symbols-for-extensions)

(defun setup-readtable ()
  (hu.dwim.quasi-quote::setup-readtable))
