;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def class* xml-syntax-node (syntax-node)
  ((name)))

(def (class* e) xml-element (xml-syntax-node)
  ((attributes nil)
   (children nil)))

(def (class* e) xml-attribute (xml-syntax-node)
  ((value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

(def (function e) transform-quasi-quoted-xml-to-quasi-quoted-string (qq-xml)
  (etypecase qq-xml
    (quasi-quote
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
       (make-instance 'quasi-quote
                      :body (bind ((body (body-of qq-xml)))
                              (if (consp body)
                                  (mapcar #'process body)
                                  (list (process (body-of qq-xml))))))))
    (unquote
     (break))))

(def (function e) expand-quasi-quoted-xml-to-lambda-form (qq-xml)
  (expand-quasi-quoted-string-to-lambda-form
   (transform-quasi-quoted-xml-to-quasi-quoted-string qq-xml)))

(def (function e) transform-quasi-quoted-xml-to-string (qq-xml)
  (transform-quasi-quoted-string-to-string
   (transform-quasi-quoted-xml-to-quasi-quoted-string qq-xml)))
