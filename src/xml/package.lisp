;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-xml
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :cl-def
        :closer-mop
        :cl-syntax-sugar
        :cl-quasi-quote
        :babel
        :babel-streams
        )

  (:export
   ))

(in-package :cl-quasi-quote-xml)

(def (function e) import-external-quasi-quote-symbols-for-extensions/xml (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of cl-quasi-quote-xml but not to its users."
  (import
   '(children-of
     name-of
     attributes-of
     )
   package))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import-external-quasi-quote-symbols-for-extensions))

(defun transform-function-definer-options (options)
  (cl-quasi-quote::transform-function-definer-options options))

(defun setup-readtable ()
  (cl-quasi-quote::setup-readtable))
