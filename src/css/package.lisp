;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-quasi-quote)

(defpackage :cl-quasi-quote-css
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
   #:print-quasi-quoted-css))

(in-package :cl-quasi-quote-css)

(def (function e) import-external-quasi-quote-symbols-for-extensions/css (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of cl-quasi-quote-css but not to its users."
  (import
   '(children-of
     name-of
     attributes-of
     )
   package))

(import-external-quasi-quote-symbols-for-extensions)

(defun transform-function-definer-options (options)
  (cl-quasi-quote::transform-function-definer-options options))

(defun setup-readtable ()
  (cl-quasi-quote::setup-readtable))
