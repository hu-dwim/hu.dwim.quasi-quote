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

(def method print-object ((self xml-quasi-quote) *standard-output*)
  (format t "`xml~A" (body-of self))
  self)

(def (function e) make-xml-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'xml-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) xml-unquote (unquote xml-syntax-node)
  ())

(def (function e) make-xml-unquote (form &optional modifier)
  (make-instance 'xml-unquote :form form :modifier modifier))

(def (class* e) xml-element (xml-syntax-node)
  ((name)
   (attributes nil :documentation "A list of xml-attribute nodes.")
   (children nil)))

(def method print-object ((self xml-element) *standard-output*)
  (pprint-logical-block (nil nil :prefix "<" :suffix ">")
    (princ (name-of self))
    (when (attributes-of self)
      (write-char #\Space)
      (bind ((attributes (iter (for attribute :in (attributes-of self))
                               (collect (if (typep attribute 'xml-attribute)
                                            (list (name-of attribute) (value-of attribute))
                                            attribute))))
             (width (iter (for attribute :in attributes)
                          (when (and (consp attribute)
                                     (stringp (first attribute)))
                            (maximize (length (first attribute))))))
             (format-control (concatenate 'string "~" (if width (princ-to-string width) "5") "A ~A")))
        (pprint-logical-block (nil attributes :prefix "(" :suffix ")")
          (pprint-exit-if-list-exhausted)
          (iter (for attribute = (pprint-pop))
                (unless (first-time-p)
                  (pprint-newline :mandatory))
                (if (consp attribute)
                    (apply #'format t format-control attribute)
                    (princ attribute))
                (pprint-exit-if-list-exhausted)))))
    (pprint-indent :block 2)
    (when (children-of self)
      (pprint-newline :mandatory)
      (pprint-logical-block (nil (children-of self))
        (pprint-exit-if-list-exhausted)
        (iter
          (unless (first-time-p)
            (pprint-newline :mandatory))
          (write (pprint-pop))
          (pprint-exit-if-list-exhausted))))))

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
