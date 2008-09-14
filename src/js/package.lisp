;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-js
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :closer-mop
        :cl-def
        :cl-walker
        :cl-syntax-sugar
        :cl-quasi-quote
        :babel
        :babel-streams
        )

  (:shadowing-import-from :cl-quasi-quote
   #:body-of
   #:parent-of)

  (:shadow
   #:catch-form)

  (:export
   #:|finally|
   ))

(in-package :cl-quasi-quote-js)

(import-external-quasi-quote-symbols-for-extensions)

(enable-readtime-wrapper-syntax)

(export {with-preserved-readtable-case
         '(nil t and or not
           if when unless cond
           progn let let*
           vector list aref elt slot-value
           setf setq incf decf
           defun lambda map mapcar map-into
           block return
           create array
           do
           &optional &key &allow-other-keys &rest &body)}
        :cl-quasi-quote-js)

(defun transform-function-definer-options (options)
  (cl-quasi-quote::transform-function-definer-options options))

(defun setup-readtable ()
  (cl-quasi-quote::setup-readtable))
