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

(def (function e) import-external-quasi-quote-symbols-for-extensions (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of cl-quasi-quote but not to its users."
  (import-duplicate-symbols)
  (import
   '(form-of
     modifier-of
     parent-of
     spliced?
     destructively-spliced?
     body-of
     syntax-node
     map-ast
     bq-process bq-bracket
     *bq-list*
     *bq-list**
     *bq-nconc*
     *bq-quote*
     *bq-append*
     *bq-simplify*
     *bq-clobberable*
     make-string-of-spaces
     find-ancestor-syntax-node
     list-without-nils
     binary-position
     ub8-vector
     vector-extend
     wrap-forms-with-bindings
     quoted-symbol?
     self-evaluating?
     maybe-slurp-in-toplevel-quasi-quote
     map-tree map-filtered-tree
     princ-to-string-unless-nil integer-to-string
     self-evaluating?
     delayed-emitting
     as-delayed-emitting
     wrap-runtime-delayed-transformation-form
     transformation-typecase
     trace-transformation-functions
     transform
     transform*
     -environment-
     -transformation-
     transformation-pipeline
     transformation-pipeline-of
     *transformation*
     *transformation-pipeline*
     *transformation-environment*
     compatible-transformation-pipelines?
     compatible-transformations?
     run-transformation-pipeline
     toplevel-quasi-quote-macro
     print-object/quasi-quote
     lisp-form-emitting-transformation
     make-syntax-node-emitting-form
     collect-slots-for-syntax-node-emitting-form
     )
   package))
