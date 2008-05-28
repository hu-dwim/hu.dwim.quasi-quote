;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

;; A quasi quoted XML AST is made of list, xml-syntax-nodes, xml-quasi-quote, xml-unquote recursively

(def ast xml)

(def class* xml-syntax-node (syntax-node)
  ())

(def (class* e) xml-quasi-quote (quasi-quote xml-syntax-node)
  ())

(def (function e) make-xml-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'xml-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) xml-unquote (unquote xml-syntax-node)
  ())

(def (function e) make-xml-unquote (form &optional (spliced? #f))
  (make-instance 'xml-unquote :form form :spliced spliced?))

(def (class* e) xml-element (xml-syntax-node)
  ((name)
   (attributes nil :documentation "A list of xml-attribute nodes.")
   (children nil)))

(def (class* e) xml-attribute (xml-syntax-node)
  ((name)
   (value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

(def (function e) make-xml-element (name &optional attributes children)
  (make-instance 'xml-element :name name :attributes attributes :children children))

(def (function e) make-xml-attribute (name value)
  (make-instance 'xml-attribute :name name :value value))

(def (function e) make-xml-text (content)
  (make-instance 'xml-text :content content))
