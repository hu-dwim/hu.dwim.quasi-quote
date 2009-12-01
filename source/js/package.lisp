;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.quasi-quote.js
  (:use :babel
        :babel-streams
        :contextl
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.syntax-sugar
        :hu.dwim.walker)
  (:shadowing-import-from :hu.dwim.quasi-quote
                          #:body-of
                          #:parent-of
                          #:map-ast)
  (:shadow #:catch-form)
  (:export #:|finally|
           #:|with|)
  (:readtable-setup (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.quasi-quote)))

(in-package :hu.dwim.quasi-quote.js)

(import-external-quasi-quote-symbols-for-extensions)

(export {with-preserved-readtable-case
         '(nil t and or not
           if when unless cond
           progn let let*
           vector list aref elt slot-value
           setf setq null incf decf
           defun lambda map mapcar map-into
           block return
           create array
           do
           &optional &key &allow-other-keys &rest &body)}
        :hu.dwim.quasi-quote.js)

