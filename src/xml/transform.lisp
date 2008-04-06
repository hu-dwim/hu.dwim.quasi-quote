;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'xml-quasi-quote fn))

(def special-variable *xml-indent-level* 0)

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/element (node &rest args &key (indent #f) &allow-other-keys)
  (bind ((indent-level
          (when indent
            (list (make-string (* indent *xml-indent-level*) :initial-element #\Space))))(indent-new-line
          (when indent
            (list #\NewLine))))
    (etypecase node
      (function node)
      (string `(,@indent-level ,(escape-as-xml node) ,@indent-new-line))
      (xml-element
       (bind ((attributes (attributes-of node))
              (name (name-of node))
              (children (children-of node)))
         `(,@indent-level
           "<" ,(etypecase name
                           (xml-unquote (make-string-unquote (form-of name)))
                           (unquote name)
                           (string name))
           ,@(when attributes
                   `(" "
                     ,@(typecase attributes
                                 (xml-unquote (list (make-string-unquote `(mapcar 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
                                                                                  ,(form-of attributes)))))
                                 (unquote attributes)
                                 (t (iter (for attribute :in attributes)
                                          (unless (first-iteration-p)
                                            (collect " "))
                                          (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))))))
           ,@(if children
                 `(">" ,@indent-new-line
                       ,@(mapcar (lambda (child)
                                   (bind ((*xml-indent-level* (1+ *xml-indent-level*)))
                                     (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element child args)))
                                 children)
                       (,@indent-level "</" ,(name-of node) ">" ,@indent-new-line))
                 `("/>" ,@indent-new-line)))))
      (xml-text
       (bind ((content (content-of node)))
         `(,@indent-level
           ,(etypecase content
                       (xml-unquote (make-string-quasi-quote (form-of node)))
                       (string (escape-as-xml content)))
           ,@indent-new-line)))
      (xml-quasi-quote
       (make-string-quasi-quote (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element (body-of node) args)))
      (xml-unquote
       (bind ((spliced? (spliced-p node)))
         (make-string-unquote
          (if spliced?
              `(map 'list (lambda (node)
                            ,(wrap-forms-with-bindings
                              (when indent `((*xml-indent-level* ,*xml-indent-level*)))
                              `(transform-quasi-quoted-xml-to-quasi-quoted-string/element node ,@args)))
                    ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                      node (lambda (node)
                             (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element node args))))
              (wrap-forms-with-bindings
               (when indent `((*xml-indent-level* ,*xml-indent-level*)))
               `(transform-quasi-quoted-xml-to-quasi-quoted-string/element
                 ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                   node (lambda (node)
                          (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element node args)))
                 ,@args)))
          spliced?)))
      (quasi-quote
       (if (typep node 'string-quasi-quote)
           (body-of node)
           node))
      (unquote (transform 'quasi-quoted-string node))
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
                                    `(escape-as-xml
                                      (princ-to-string
                                       ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                         value #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))))
                      (unquote value)
                      (string (escape-as-xml value)))
          "\"")))
    (xml-quasi-quote
     (make-string-quasi-quote (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
    (xml-unquote
     (bind ((spliced? (spliced-p node)))
       (make-string-unquote
        (if spliced?
            `(iter (for attribute :in-sequence ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                                 node #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute))
                   (unless (first-iteration-p)
                     (collect " "))
                   (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))
            `(transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
              ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                node #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
        spliced?)))
    (quasi-quote
     (if (typep node 'string-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-string node))))

(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element input args))
