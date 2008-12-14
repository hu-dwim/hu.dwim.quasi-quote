;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(def (transformation e) quasi-quoted-xml-to-quasi-quoted-string ()
  ((text-node-escaping-method :per-character :type (member :cdata :per-character))
   (indentation-width nil))
  'transform-quasi-quoted-xml-to-quasi-quoted-string)

(defmethod print-object ((self quasi-quoted-xml-to-quasi-quoted-string) *standard-output*)
  (princ "[XML->String]"))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string (node)
  (assert (typep node 'xml-quasi-quote))
  (make-string-quasi-quote (rest (transformation-pipeline-of node))
                           (transform-quasi-quoted-xml-to-quasi-quoted-string/element (body-of node))))

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

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/tag-name (node)
  (transformation-typecase node
    (string node)
    (integer (integer-to-string node))
    (xml-unquote (bind ((form (form-of node)))
                   (if (self-evaluating? form)
                       (transform-quasi-quoted-xml-to-quasi-quoted-string/tag-name form)
                       (make-string-unquote
                        (wrap-runtime-delayed-transformation-form form)))))
    (string-quasi-quote node)))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/element (node)
  (bind ((indent-level (awhen (indentation-width-of *transformation*)
                         (list (make-string-of-spaces (* it *xml-indent-level*)))))
         (indent-new-line (when indent-level
                            '(#\NewLine))))
    (transformation-typecase node
      (string (ecase (text-node-escaping-method-of *transformation*)
                (:cdata (wrap-with-xml-quote node))
                (:per-character (escape-as-xml node))))
      (integer (integer-to-string node))
      (float (format nil "~F" node))
      (ratio (format nil "~D" node))
      (xml-element
       (bind ((attributes (attributes-of node))
              (name (name-of node))
              (transformed-name (transform-quasi-quoted-xml-to-quasi-quoted-string/tag-name name))
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
       (bind ((spliced? (spliced? node))
              (form (form-of node)))
         (if spliced?
             (progn
               ;; TODO optimize
               )
             (progn
               ;; first eliminate a possible toplevel qq that ends up wrapped in an unquote, probably in a macro, coming from a macro argument
               (setf form (maybe-slurp-in-toplevel-quasi-quote (form-of node)))
               ;; if it's a macro call, let's see if maybe it expands to a literal qq that we can slurp in
               (when (and (consp form)
                          (symbolp (first form))
                          (macro-function (first form)))
                 ;; TODO this should be macroexpand and handle macrolet's. macroexpand is problematic because it also expands TOPLEVEL-QUASI-QUOTE-MACRO
                 (awhen (maybe-slurp-in-toplevel-quasi-quote
                         (macroexpand-1 form *transformation-environment*))
                   (setf form it)))))
         (if (self-evaluating? form)
             (transform-quasi-quoted-xml-to-quasi-quoted-string/element form)
             (make-string-unquote
              (wrap-runtime-delayed-transformation-form
               (if spliced?
                   `(map 'list (lambda (node)
                                 ,(with-runtime-xml-indent-level
                                   ;; TODO this is not enough, unquoted parts are not indented just because of this
                                   `(transform-quasi-quoted-xml-to-quasi-quoted-string/element node)))
                         ,form)
                   (with-runtime-xml-indent-level
                     `(transform-quasi-quoted-xml-to-quasi-quoted-string/element ,form))))))))
      (string-quasi-quote node)
      (null (values)))))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/attribute (node)
  (transformation-typecase node
    (xml-attribute
     (bind ((name (name-of node))
            (transformed-name (etypecase name
                                (xml-unquote (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute name))
                                (unquote name)
                                (string name)))
            (value (value-of node)))
       (if (and (typep value 'xml-unquote)
                ;; NOTE: checks if the the form is a simple variable reference, because otherwise due to inline emitting we cannot evaluate the value out of order
                (symbolp (form-of value)))
           (when-bind form (form-of value)
             (make-string-unquote
              (with-unique-names (value-variable)
                `(when-bind ,value-variable ,form
                   ,(make-string-quasi-quote (rest *transformation-pipeline*)
                                             `(,transformed-name
                                               "=\""
                                               ,(make-string-unquote `(transform-quasi-quoted-xml-to-quasi-quoted-string/attribute-value ,value-variable))
                                               "\""))))))
           `(,transformed-name
             "=\""
             ,(transform-quasi-quoted-xml-to-quasi-quoted-string/attribute-value value)
             "\""))))
    (xml-quasi-quote
     (make-string-quasi-quote (rest (transformation-pipeline-of node))
                              (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
    (xml-unquote
     (bind ((spliced? (spliced? node)))
       (make-string-unquote
        (wrap-runtime-delayed-transformation-form
         (if spliced?
             `(iter (for attribute :in-sequence ,(form-of node))
                    (unless (first-iteration-p)
                      (collect " "))
                    (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))
             `(awhen ,(form-of node)
                (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute it)))))))
    (string-quasi-quote node)))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/attribute-value (node)
  (transformation-typecase node
    (xml-unquote (make-string-unquote
                  (wrap-runtime-delayed-transformation-form
                   `(locally
                        (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                      (awhen ,(form-of node)
                        (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute-value it))))))
    (integer (integer-to-string node))
    (float (format nil "~F" node))
    (string-quasi-quote node) ;; TODO what about xml escaping?
    (string (escape-as-xml node))))

;;;;;;
;;; xml escaping

(def (transformation e) quasi-quoted-string-to-xml-escaped-quasi-quoted-string (generic-transformation)
  ()
  nil
  (:default-initargs
     :unquote-transformer 'xml-escape-unquote-transformer
     :quasi-quote-transformer 'xml-escape-quasi-quote-transformer))

(defmethod print-object ((self quasi-quoted-string-to-xml-escaped-quasi-quoted-string) *standard-output*)
  (princ "[String->XML-Escaped-String]"))

(def function make-quasi-quoted-string-to-xml-escaped-quasi-quoted-string-transformation (&key output-prefix output-postfix)
  (make-instance 'quasi-quoted-string-to-xml-escaped-quasi-quoted-string
                 :output-transformer (lambda (node)
                                       (assert (typep node 'string-quasi-quote))
                                       (awhen output-prefix
                                         (push it (body-of node)))
                                       (awhen output-postfix
                                         (appendf (body-of node) (list it)))
                                       node)))

(def function xml-escape-unquote-transformer (node)
  (check-type node string-unquote)
  (setf (form-of node) `(awhen ,(form-of node)
                          (escape-as-xml it)))
  node)

(def function xml-escape-quasi-quote-transformer (node)
  (check-type node string-quasi-quote)
  (assert (typep (first (transformation-pipeline-of node)) 'quasi-quoted-string-to-xml-escaped-quasi-quoted-string))
  (labels ((recurse (node)
             (etypecase node
               (cons (cons (recurse (car node))
                           (recurse (cdr node))))
               (null nil)
               (string (escape-as-xml node))
               (character (or (xml-escaped-entity-for-character node)
                              node))
               (unquote (xml-escape-unquote-transformer node)))))
    (setf (body-of node)
          (iter (for el :in (body-of node))
                (awhen (recurse el)
                  (collect it)))))
  (pop (transformation-pipeline-of node))
  node)
