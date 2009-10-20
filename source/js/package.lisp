;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.quasi-quote.js
  (:use :babel
        :babel-streams
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
           #:|with|))

(in-package :hu.dwim.quasi-quote.js)

(import-external-quasi-quote-symbols-for-extensions)

(enable-readtime-wrapper-syntax)

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

(defun setup-readtable ()
  (hu.dwim.quasi-quote::setup-readtable))
