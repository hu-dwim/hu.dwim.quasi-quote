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

(def method print-object ((self js-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "js"))

(def (function e) make-js-quasi-quote (transformation-pipeline body)
  (make-instance 'js-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) js-unquote (unquote js-syntax-node)
  ())

(def (function e) make-js-unquote (form &optional modifier)
  (make-instance 'js-unquote :form form :modifier modifier))

(def methods map-ast
  (:method (fn (x js-quasi-quote))
    (map-ast/map-accessors-unless-same-returned fn x
      body-of))
  (:method (fn (x walked-form))
    (cl-walker:map-ast fn x)))

(def methods bq-process
  (:method ((x js-quasi-quote))
    `(list 'toplevel-quasi-quote-macro
           (make-js-quasi-quote (quote ,(transformation-pipeline-of x))
                                ,(map-ast #'bq-process (body-of x)))))

  (:method ((x js-unquote))
    (bind ((form (form-of x)))
      (if (spliced? x)
          `(make-js-unquote (list* 'list ,(bq-bracket form)) ,(modifier-of x))
          `(make-js-unquote ,(bq-process form) ,(modifier-of x)))))

  (:method ((x walked-form))
    (bind ((class (class-of x))
           (class-name (class-name class)))
      `(make-instance ',class-name
                      ,@(iter (for slot :in (class-slots class))
                              (unless (member (slot-definition-name slot) '(cl-walker::source cl-walker::parent))
                                (bind ((initarg (first (slot-definition-initargs slot)))
                                       (slot-value (slot-value-using-class class x slot))
                                       (processed-slot-value (map-ast #'bq-process slot-value)))
                                  (assert initarg)
                                  (nconcing (list initarg (if (consp slot-value)
                                                              (cons 'list processed-slot-value)
                                                              processed-slot-value)))))))))

  (:method ((x variable-binding-form))
    `(make-instance 'variable-binding-form
                    :body (list ,@(map-ast #'bq-process (cl-walker:body-of x)))
                    :bindings (list ,@(iter (for (name . value) :in (bindings-of x))
                                            (collect `(cons ,(map-ast #'bq-process name) ,(map-ast #'bq-process value))))))))