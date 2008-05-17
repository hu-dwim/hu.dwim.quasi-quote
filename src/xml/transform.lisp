;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'xml-quasi-quote fn))

(def (special-variable e) *xml-text-escape-method* :cdata
  "Either :cdata or :per-character")

(def (special-variable e) *xml-indent* 0)
(def special-variable *xml-indent-level* 0)

(def macro with-increased-xml-indent-level (&body body)
  `(bind ((*xml-indent-level* (1+ *xml-indent-level*)))
     ,@body))

(def macro with-runtime-xml-indent-level (&body body)
  `(wrap-forms-with-bindings
    (when *xml-indent* `((*xml-indent* ,*xml-indent*)
                         (*xml-indent-level* (+ *xml-indent-level* ,*xml-indent-level*))))
    ,@body))

(def (function o) wrap-with-xml-quote (string)
  (declare (type string string))
  (list "<![CDATA[" string "]]>"))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/element (node)
  (bind ((indent-level (unless (zerop *xml-indent*)
                         (list (make-string-of-spaces (* *xml-indent* *xml-indent-level*)))))
         (indent-new-line (unless (zerop *xml-indent*)
                            '(#\NewLine))))
    (etypecase node
      (function node)
      (xml-element
       (bind ((attributes (attributes-of node))
              (name (name-of node))
              (transformed-name (etypecase name
                                  (xml-unquote (make-string-unquote (form-of name)))
                                  (unquote name)
                                  (string name)))
              (children (children-of node)))
         `(,@indent-level
           "<" ,transformed-name
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
                                   (with-increased-xml-indent-level
                                     (transform-quasi-quoted-xml-to-quasi-quoted-string/element child)))
                                 children)
                       (,@indent-level "</" ,transformed-name ">" ,@indent-new-line))
                 `("/>" ,@indent-new-line)))))
      (xml-text
       (bind ((content (content-of node)))
         (etypecase content
           (xml-unquote (make-string-unquote `(ecase *xml-text-escape-method*
                                                (:cdata (wrap-with-xml-quote ,(form-of node)))
                                                (:per-character (escape-as-xml ,(form-of node))))))
           (string (ecase *xml-text-escape-method*
                     (:cdata (wrap-with-xml-quote content))
                     (:per-character (escape-as-xml content)))))))
      (xml-quasi-quote
       (make-string-quasi-quote (transform-quasi-quoted-xml-to-quasi-quoted-string/element (body-of node))))
      (xml-unquote
       (bind ((spliced? (spliced-p node)))
         (make-string-unquote
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
                          (transform-quasi-quoted-xml-to-quasi-quoted-string/element node))))))
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

(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &key indent &allow-other-keys)
  (bind ((*xml-indent* (or indent *xml-indent*)))
    (transform-quasi-quoted-xml-to-quasi-quoted-string/element input)))
