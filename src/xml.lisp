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
                                       (bind (((name attributes &rest children) form))
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
                                                        :children (mapcar #'process children))))))
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

;;;;;;;
;;; AST

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

;; TODO: escaping
(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &key &allow-other-keys)
  (labels ((process-unquote (node)
             (map-tree (form-of node)
                       (lambda (form)
                         (if (typep form 'xml-quasi-quote)
                             (process form)
                             form))))
           (process (node)
             (etypecase node
               (xml-element
                (bind ((attributes (attributes-of node))
                       (children (children-of node)))
                  `("<" ,(name-of node)
                        ,@(when attributes
                                `(" " ,@(if (typep attributes 'xml-unquote)
                                            (list (process attributes))
                                            (iter (for attribute :in attributes)
                                                  (unless (first-iteration-p)
                                                    (collect " "))
                                                  (collect (process attribute))))))
                        ,(if children
                             `(">"
                               ,@(mapcar #'process children)
                               ("</" ,(name-of node) ">"))
                             "/>"))))
               (xml-attribute
                (bind ((name (name-of node))
                       (value (value-of node)))
                  `(,(if (typep name 'xml-unquote)
                         (process name)
                         name)
                     "=\""
                     ,(if (typep value 'xml-unquote)
                          (make-instance 'string-unquote
                                         :form `(princ-to-string ,(process-unquote value)))
                          (princ-to-string value))
                     "\"")))
               (xml-text `
                ("<!CDATA[["
                 ,(if (typep node 'xml-unquote)
                      (process node)
                      (content-of node))
                 "]]>"))
               (xml-quasi-quote
                (make-instance 'string-quasi-quote
                               :body (map-tree (body-of node) #'process)))
               (xml-unquote
                (make-instance 'string-unquote
                               :form `(funcall ,#'process ,(process-unquote node)))))))
    (process input)))
