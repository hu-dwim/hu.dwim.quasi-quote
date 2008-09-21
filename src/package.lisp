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
     parent-of
     spliced-p
     body-of
     syntax-node
     make-string-of-spaces
     bql bqnc
     find-ancestor-syntax-node
     binary-position
     ub8-vector
     vector-extend
     wrap-forms-with-bindings
     quoted-symbol?
     map-tree map-filtered-tree
     princ-to-string-unless-nil
     reader-stub
     delayed-emitting
     as-delayed-emitting
     wrap-runtime-delayed-transformation-form
     transformation-typecase
     transform
     -environment-
     -transformation-
     transformation-pipeline
     transformation-pipeline-of
     *transformation*
     *transformation-pipeline*
     compatible-transformation-pipelines?
     compatible-transformations?
     run-transformation-pipeline
     recursively-macroexpand-reader-stubs
     lisp-form-emitting-transformation
     make-syntax-node-emitting-form
     collect-slots-for-syntax-node-emitting-form
     )
   package))
