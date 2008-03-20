;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(def (function e) with-quasi-quoted-xml-syntax ()
  (lambda (reader)
    (set-quasi-quote-syntax-in-readtable
     (lambda (body) (make-instance 'xml-quasi-quote
                              :body (if (= 1 (length body))
                                        (make-instance 'xml-element :name (string-downcase (first body)))
                                        body)))
     (lambda (form spliced) (make-instance 'xml-unquote :form form :spliced spliced))
     :quasi-quote-character #\<
     :quasi-quote-end-character #\>)
    (first (funcall reader))))

;;;;;;;
;;; AST

(def ast xml)

(def class* xml-syntax-node (syntax-node)
  ((name)))

(def class* xml-quasi-quote (quasi-quote xml-syntax-node)
  ())

(def class* xml-unquote (unquote xml-syntax-node)
  ())

(def (class* e) xml-element (xml-syntax-node)
  ((attributes nil)
   (children nil)))

(def (class* e) xml-attribute (xml-syntax-node)
  ((value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'string)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string (transform 'quasi-quoted-string input) args))

(def method transform ((to (eql 'string-emitting-form)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string-emitting-form (transform 'quasi-quoted-string input) args))

;; TODO: escaping
(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &key &allow-other-keys)
  (etypecase input
    (xml-quasi-quote
     (labels ((process (node)
                (etypecase node
                  (xml-element (bind ((attributes (attributes-of node))
                                      (children (children-of node)))
                                 `("<" ,(name-of node)
                                       ,@(when attributes
                                               `(" " ,@(iter (for attribute :in attributes)
                                                             (unless (first-iteration-p)
                                                               (collect " "))
                                                             (collect (process attribute)))))
                                       ,(if children
                                            `(">"
                                              ,@(mapcar #'process children)
                                              ("</" ,(name-of node) ">"))
                                            "/>"))))
                  (xml-attribute `(,(name-of node) "=\"" ,(princ-to-string (value-of node)) "\""))
                  (xml-text `("<!CDATA[[" ,(content-of node) "]]>")))))
       (make-instance 'string-quasi-quote
                      :body (map-tree (body-of input) #'process))))
    (xml-unquote
     (break))))
