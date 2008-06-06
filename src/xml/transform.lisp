;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(def (transformation e) quasi-quoted-xml-to-quasi-quoted-string ()
  ((text-node-escaping-method :per-character :type (member :cdata :per-character))
   (indentation-width nil))
  'transform-quasi-quoted-xml-to-quasi-quoted-string/element)

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
    (transformation-typecase node
      (string (ecase (text-node-escaping-method-of *transformation*)
                (:cdata (wrap-with-xml-quote node))
                (:per-character (escape-as-xml node))))
      (integer (princ-to-string node))
      (float (format nil "~F" node))
      (xml-element
       (bind ((attributes (attributes-of node))
              (name (name-of node))
              (transformed-name (etypecase name
                                  (xml-unquote (if (and (consp (form-of name))
                                                        (eq (first (form-of name)) 'progn)
                                                        (every #'stringp (rest (form-of name))))
                                                   ;; TODO this optimization may be superfluous once the list qq code is revived... check, delete.
                                                   (apply #'concatenate 'string (rest (form-of name)))
                                                   (make-string-unquote
                                                    (wrap-runtime-delayed-transformation-form
                                                     (form-of name)))))
                                  (string name)))
              (children (children-of node)))
         `(,@indent-level
           "<" ,transformed-name
           ,@(when (and attributes
                        (or (consp attributes)
                            (not (zerop (length attributes)))))
                   `(" "
                     ,@(typecase attributes
                                 (xml-unquote (make-string-unquote
                                               (wrap-runtime-delayed-transformation-form
                                                `(mapcar 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
                                                         ,(form-of attributes)))))
                                 (unquote attributes)
                                 (t (iter (for attribute :in-sequence attributes)
                                          (unless (first-iteration-p)
                                            (collect " "))
                                          (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))))))
           ,@(if children
                 `(">" ,@indent-new-line
                       ,@(map 'list (lambda (child)
                                      (with-increased-xml-indent-level
                                        (transform-quasi-quoted-xml-to-quasi-quoted-string/element child)))
                              children)
                       (,@indent-level "</" ,transformed-name ">" ,@indent-new-line))
                 `("/>" ,@indent-new-line)))))
      (xml-text
       (bind ((content (content-of node)))
         (etypecase content
           (xml-unquote (make-string-unquote
                         (wrap-runtime-delayed-transformation-form
                          `(ecase (text-node-escaping-method-of *transformation*)
                             (:cdata (wrap-with-xml-quote ,(form-of node)))
                             (:per-character (escape-as-xml ,(form-of node)))))))
           (string (ecase (text-node-escaping-method-of *transformation*)
                     (:cdata (wrap-with-xml-quote content))
                     (:per-character (escape-as-xml content)))))))
      (xml-quasi-quote
       (if (compatible-transformation-pipelines? *transformation-pipeline*
                                                 (transformation-pipeline-of node))
           (make-string-quasi-quote (rest (transformation-pipeline-of node))
                                    (transform-quasi-quoted-xml-to-quasi-quoted-string/element (body-of node)))
           (transform node)))
      (xml-unquote
       (bind ((spliced? (spliced-p node)))
         (make-string-unquote
          (wrap-runtime-delayed-transformation-form
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
      (string-quasi-quote node)
      (null (values)))))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/attribute (node)
  (transformation-typecase node
    (xml-attribute
     (bind ((name (name-of node))
            (value (value-of node)))
       `(,(etypecase name
                     (xml-unquote (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute name))
                     (unquote name)
                     (string name))
          "=\""
          ,(transformation-typecase value
             (xml-unquote (make-string-unquote
                           (wrap-runtime-delayed-transformation-form
                            `(locally
                                 (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                               (awhen ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                        value #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)
                                 (escape-as-xml (princ-to-string it)))))))
             (string-quasi-quote value) ;; TODO what about xml escaping?
             (string (escape-as-xml value)))
          "\"")))
    (xml-quasi-quote
     (make-string-quasi-quote (rest (transformation-pipeline-of node))
                              (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
    (xml-unquote
     (bind ((spliced? (spliced-p node)))
       (make-string-unquote
        (wrap-runtime-delayed-transformation-form
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
    (string-quasi-quote node)))

