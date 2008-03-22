;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax (quasi-quoted-xml :readtime-wrapper-result-transformer (lambda (result)
                                                                        (if (rest result)
                                                                            (make-instance 'xml-quasi-quote :body (mapcar 'body-of result))
                                                                            (first result))))
    (&key (quasi-quote-character #\<)
          (quasi-quote-end-character #\>)
          (unquote-character #\,)
          (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (make-instance 'xml-quasi-quote
                    :body (labels ((process (form)
                                     (macrolet ((unless-unquote (value &body forms)
                                                  (once-only (value)
                                                    `(if (typep ,value 'xml-unquote)
                                                         ,value
                                                         (progn
                                                           ,@forms)))))
                                       (etypecase form
                                         (string
                                          (make-instance 'xml-text :content form))
                                         (cons
                                          (bind (((name &optional attributes &rest children) form))
                                            (make-instance 'xml-element
                                                           :name (name-as-string name)
                                                           :attributes (unless-unquote attributes
                                                                                       (iter (generate element :in attributes)
                                                                                             (for name = (next element))
                                                                                             (if (typep name 'xml-unquote)
                                                                                                 (collect name)
                                                                                                 (bind ((value (next element)))
                                                                                                   (collect (make-instance 'xml-attribute
                                                                                                                           :name (unless-unquote name (name-as-string name))
                                                                                                                           :value (unless-unquote value (princ-to-string value))))))))
                                                           :children (mapcar #'process children))))
                                         (xml-unquote form)))))
                            (process body))))
   (lambda (form spliced) (make-instance 'xml-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :quasi-quote-reader-wrapper
   (lambda (original-reader)
     (lambda (stream char)
       (bind ((*readtable* (copy-readtable)))
         (set-macro-character quasi-quote-character
                              (lambda (stream char)
                                (declare (ignore char))
                                (read-delimited-list #\> stream t)))
         (funcall original-reader stream char))))))

(def (function e) with-quasi-quoted-xml-to-binary-syntax (&key stream)
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'quasi-quoted-string 'quasi-quoted-binary
                                        `(binary-emitting-form ,@(when stream
                                                                       `(:stream ,stream)))))

(def (function e) with-quasi-quoted-xml-to-string-syntax ()
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'quasi-quoted-string 'string-emitting-form))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted XML is made of list, xml-syntax-nodes, xml-quasi-quote, xml-unquote recursively

(def ast xml)

(def class* xml-syntax-node (syntax-node)
  ())

(def class* xml-quasi-quote (quasi-quote xml-syntax-node)
  ())

(def class* xml-unquote (unquote xml-syntax-node)
  ())

(def (class* e) xml-element (xml-syntax-node)
  ((name)
   (attributes nil)
   (children nil)))

(def (class* e) xml-attribute (xml-syntax-node)
  ((name)
   (value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'string)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string (transform 'quasi-quoted-string input) args))

(def method transform ((to (eql 'string-emitting-form)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string-emitting-form (transform 'quasi-quoted-string input) args))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string (node)
  (flet ((process-xml-unqoute-form (node)
           (map-tree (form-of node)
                     (lambda (form)
                       (if (typep form 'xml-quasi-quote)
                           (transform-quasi-quoted-xml-to-quasi-quoted-string form)
                           form)))))
    (etypecase node
      (xml-element
       (bind ((attributes (attributes-of node))
              (children (children-of node)))
         `("<" ,(name-of node)
               ,@(when attributes
                       `(" " ,@(if (typep attributes 'xml-unquote)
                                   (list (make-instance 'string-unquote
                                                        :form `(mapcar 'transform-quasi-quoted-xml-to-quasi-quoted-string ,(form-of attributes))))
                                   (iter (for attribute :in attributes)
                                         (unless (first-iteration-p)
                                           (collect " "))
                                         (collect (transform-quasi-quoted-xml-to-quasi-quoted-string attribute))))))
               ,(if children
                    `(">"
                      ,@(mapcar #'transform-quasi-quoted-xml-to-quasi-quoted-string children)
                      ("</" ,(name-of node) ">"))
                    "/>"))))
      (xml-attribute
       (bind ((name (name-of node))
              (value (value-of node)))
         `(,(if (typep name 'xml-unquote)
                (transform-quasi-quoted-xml-to-quasi-quoted-string name)
                name)
            "=\""
            ;; TODO: escaping
            ,(if (typep value 'xml-unquote)
                 (make-instance 'string-unquote
                                :form `(princ-to-string ,(process-xml-unqoute-form value)))
                 (princ-to-string value))
            "\"")))
      (xml-text
       (bind ((content (content-of node)))
         (if (typep content 'xml-unquote)
             (transform-quasi-quoted-xml-to-quasi-quoted-string content)
             content)
         ;; TODO: escaping
         #+nil
         ("<!CDATA[["
          ,content
          "]]>")))
      (xml-quasi-quote
       (make-instance 'string-quasi-quote
                      :body (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string)))
      (xml-unquote
       (make-instance 'string-unquote
                      :form `(transform-quasi-quoted-xml-to-quasi-quoted-string ,(process-xml-unqoute-form node))))
      (string node)
      (number (princ-to-string node)))))

(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-xml-to-quasi-quoted-string input))
