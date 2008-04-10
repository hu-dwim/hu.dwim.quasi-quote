;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

;; The quasi quoted JavaScript AST is the cl-walker AST plus js-quasi-quote, js-unquote

(def ast js)

;; TODO move these into def ast?
(def class* js-syntax-node (syntax-node)
  ())

(def (class* e) js-quasi-quote (quasi-quote js-syntax-node)
  ())

(def (function e) make-js-quasi-quote (body)
  (make-instance 'js-quasi-quote :body body))

(def (class* e) js-unquote (unquote js-syntax-node)
  ())

(def (function e) make-js-unquote (form &optional (spliced? #f))
  (make-instance 'js-unquote :form form :spliced spliced?))

