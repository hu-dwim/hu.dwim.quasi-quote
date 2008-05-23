;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-quasi-quote
  (:nicknames :qq)

  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :defclass-star
        :closer-mop
        :cl-def
        :cl-syntax-sugar
        :babel
        :babel-streams
        )

  (:export))

(in-package :cl-quasi-quote)

(def function import-semi-external-quasi-quote-symbols (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of cl-quasi-quote but not to its users."
  (import-duplicate-symbols)
  (import
   '(form-of
     parent-of
     spliced-p
     body-of
     syntax-node
     make-string-of-spaces
     find-ancestor
     binary-position
     ub8-vector
     vector-extend
     wrap-forms-with-bindings
     quoted-symbol?
     delayed-emitting
     as-delayed-emitting
     wrap-runtime-delayed-transformation-form
     transformation-typecase
     transform
     -transformation-
     transformation-pipeline
     transformation-pipeline-of
     *transformation*
     *transformation-pipeline*
     compatible-transformation-pipelines?
     compatible-transformations?
     run-transformation-pipeline
     recursively-macroexpand-reader-stubs
     )
   package))
