;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(def class* quasi-quoted-xml-to-quasi-quoted-string (transformation)
  ((text-node-escaping-method :per-character :type (member :cdata :per-character))
   (indentation-width nil))
  (:metaclass funcallable-standard-class))

(def constructor quasi-quoted-xml-to-quasi-quoted-string
  (set-funcallable-instance-function self (lambda (node)
                                            (transform-quasi-quoted-xml-to-quasi-quoted-string/element node))))

(def special-variable *xml-indent-level* 0)

(def macro with-increased-xml-indent-level (&body body)
  `(bind ((*xml-indent-level* (1+ *xml-indent-level*)))
     ,@body))

(def macro with-runtime-xml-indent-level (&body body)
  `(wrap-forms-with-bindings
    (when (indentation-width-of *transformation*)
      `((*xml-indent-level* (+ *xml-indent-level* ,*xml-indent-level*))))
    ,@body))

(def (function o) wrap-with-xml-quote (string)
  (declare (type string string))
  (list "<![CDATA[" string "]]>"))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'xml-quasi-quote fn))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/element (node)
  (bind ((indent-level (awhen (indentation-width-of *transformation*)
                         (list (make-string-of-spaces (* it *xml-indent-level*)))))
         (indent-new-line (when indent-level
                            '(#\NewLine))))
    (etypecase node
      (function node)
      (string (escape-as-xml node))
      (xml-element
       (bind ((attributes (attributes-of node))
              (name (name-of node))
              (transformed-name (etypecase name
                                  (xml-unquote (make-string-unquote
                                                (wrap-transformation-form-delayed-to-runtime
                                                 (form-of name))))
                                  (unquote name)
                                  (string name)))
              (children (children-of node)))
         `(,@indent-level
           "<" ,transformed-name
           ,@(when attributes
                   `(" "
                     ,@(typecase attributes
                                 (xml-unquote (list (make-string-unquote
                                                     (wrap-transformation-form-delayed-to-runtime
                                                      `(mapcar 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
                                                               ,(form-of attributes))))))
                                 (unquote attributes)
                                 (t (iter (for attribute :in attributes)
                                          (unless (first-iteration-p)
                                            (collect " "))
                                          (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))))))
           ,@(if children
                 `(">" ,@indent-new-line
                       ,@(mapcar (lambda (child)
                                   (with-increased-xml-indent-level
                                     (transform-quasi-quoted-xml-to-quasi-quoted-string/element child)))
                                 children)
                       (,@indent-level "</" ,transformed-name ">" ,@indent-new-line))
                 `("/>" ,@indent-new-line)))))
      (xml-text
       (bind ((content (content-of node)))
         (etypecase content
           (xml-unquote (make-string-unquote
                         (wrap-transformation-form-delayed-to-runtime
                          `(ecase (text-node-escaping-method-of *transformation*)
                             (:cdata (wrap-with-xml-quote ,(form-of node)))
                             (:per-character (escape-as-xml ,(form-of node)))))))
           (string (ecase (text-node-escaping-method-of *transformation*)
                     (:cdata (wrap-with-xml-quote content))
                     (:per-character (escape-as-xml content)))))))
      (xml-quasi-quote
       (make-string-quasi-quote (rest (transformation-pipeline-of node))
                                (transform-quasi-quoted-xml-to-quasi-quoted-string/element (body-of node))))
      (xml-unquote
       (bind ((spliced? (spliced-p node)))
         (make-string-unquote
          (wrap-transformation-form-delayed-to-runtime
           (if spliced?
               `(map 'list (lambda (node)
                             ,(with-runtime-xml-indent-level
                               ;; TODO this is not enough, unquoted parts are not indented just because of this
                               `(transform-quasi-quoted-xml-to-quasi-quoted-string/element node)))
                     ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                       node (lambda (node)
                              (transform-quasi-quoted-xml-to-quasi-quoted-string/element node))))
               (with-runtime-xml-indent-level
                 `(transform-quasi-quoted-xml-to-quasi-quoted-string/element
                   ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                     node (lambda (node)
                            (transform-quasi-quoted-xml-to-quasi-quoted-string/element node)))))))
          spliced?)))
      #+nil ;; TODO
      (quasi-quote
       (if (typep node 'string-quasi-quote)
           (body-of node)
           node))
      ;; TODO ? (unquote (transform 'quasi-quoted-string node))
      (side-effect node))))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/attribute (node)
  (etypecase node
    (function node)
    (xml-attribute
     (bind ((name (name-of node))
            (value (value-of node)))
       `(,(etypecase name
                     (xml-unquote (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute name))
                     (unquote name)
                     (string name))
          "=\""
          ,(etypecase value
                      (xml-unquote (make-string-unquote
                                    (wrap-transformation-form-delayed-to-runtime
                                     `(escape-as-xml
                                       (princ-to-string
                                        ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                          value #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute))))))
                      (unquote value)
                      (string (escape-as-xml value)))
          "\"")))
    (xml-quasi-quote
     (make-string-quasi-quote (rest (transformation-pipeline-of node))
                              (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
    (xml-unquote
     (bind ((spliced? (spliced-p node)))
       (make-string-unquote
        (wrap-transformation-form-delayed-to-runtime
         (if spliced?
             `(iter (for attribute :in-sequence ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                                  node #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute))
                    (unless (first-iteration-p)
                      (collect " "))
                    (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))
             `(transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
               ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                 node #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute))))
        spliced?)))
    (quasi-quote
     (if (typep node 'string-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-string node))))

