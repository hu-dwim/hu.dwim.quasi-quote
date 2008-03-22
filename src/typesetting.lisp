;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

;; TODO: separate package?
(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(def function typesetting-syntax-node-name (name)
  (format-symbol *package* "TYPESETTING-~A" name))

(define-syntax quasi-quoted-typesetting (&key (quasi-quote-character #\[)
                                              (quasi-quote-end-character #\])
                                              (unquote-character #\,)
                                              (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (make-instance 'typesetting-quasi-quote :body (parse-quasi-quoted-typesetting body)))
   (lambda (form spliced)
     (make-instance 'typesetting-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(def (function e) with-quasi-quoted-typesetting-to-string-syntax ()
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'quasi-quoted-xml 'quasi-quoted-string 'string-emitting-form))

(def (function e) with-quasi-quoted-typesetting-to-binary-syntax ()
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'quasi-quoted-xml 'quasi-quoted-string 'quasi-quoted-binary `binary-emitting-form))

(def (function e) with-quasi-quoted-typesetting-to-binary-stream-syntax (stream)
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-typesetting 'quasi-quoted-xml 'quasi-quoted-string 'quasi-quoted-binary `(binary-emitting-form :stream ,stream)))

(def function parse-quasi-quoted-typesetting (form)
  (if (typep form 'typesetting-unquote)
      form
      (parse-quasi-quoted-typesetting* (typesetting-syntax-node-name (first form)) form)))

(defgeneric parse-quasi-quoted-typesetting* (first whole))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-screen)) whole)
  (make-instance 'typesetting-screen
                 :content (parse-quasi-quoted-typesetting (second whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-list)) whole)
  (make-instance 'typesetting-list
                 :elements (mapcar 'parse-quasi-quoted-typesetting (cdr whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-vertical-list)) whole)
  (make-instance 'typesetting-vertical-list
                 :elements (mapcar 'parse-quasi-quoted-typesetting (cdr whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-horizontal-list)) whole)
  (make-instance 'typesetting-horizontal-list
                 :elements (mapcar 'parse-quasi-quoted-typesetting (cdr whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-paragraph)) whole)
  (make-instance 'typesetting-paragraph
                 :contents (cdr whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-menu)) whole)
  (make-instance 'typesetting-menu
                 :menu-items (mapcar 'parse-quasi-quoted-typesetting (cdr whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-menu-item)) whole)
  (make-instance 'typesetting-menu-item
                 :label (second whole)
                 :action (third whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-button)) whole)
  (make-instance 'typesetting-button
                 :label (second whole)
                 :action (third whole)))

;;;;;;;
;;; AST

(def ast typesetting)

(def class* typesetting-syntax-node (syntax-node)
  ())

(def class* typesetting-quasi-quote (quasi-quote typesetting-syntax-node)
  ())

(def class* typesetting-unquote (unquote typesetting-syntax-node)
  ())

(def (class* e) typesetting-screen (typesetting-syntax-node)
  ((content)))

(def (class* e) typesetting-list (typesetting-syntax-node)
  ((orientation :vertical :type (member :horizontal :vertical))
   (elements)))

(def (class* e) typesetting-vertical-list (typesetting-list)
  ()
  (:default-initargs :orientation :vertical))

(def (class* e) typesetting-horizontal-list (typesetting-list)
  ()
  (:default-initargs :orientation :horizontal))

(def (class* e) typesetting-paragraph (typesetting-syntax-node)
  ((contents)))

(def (class* e) typesetting-menu (typesetting-syntax-node)
  ((menu-items)))

(def (class* e) typesetting-menu-item (typesetting-syntax-node)
  ((label)
   (action)))

(def (class* e) typesetting-button (typesetting-syntax-node)
  ((label)
   (action)))

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'string)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string (transform 'quasi-quoted-xml input) args))

(def method transform ((to (eql 'string-emitting-form)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string-emitting-form (transform 'quasi-quoted-xml input) args))

(def method transform ((to (eql 'quasi-quoted-xml)) (input typesetting-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-typesetting-to-quasi-quoted-xml input))

(defgeneric transform-quasi-quoted-typesetting-to-quasi-quoted-xml (node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node string))
  node)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node number))
  (princ-to-string node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-quasi-quote))
  (make-instance 'xml-quasi-quote
                 :body (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (body-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-unquote))
  (make-instance 'xml-unquote
                 :form `(transform-quasi-quoted-typesetting-to-quasi-quoted-xml ,(form-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-screen))
  (make-instance 'xml-element
                 :name "html"
                 :children (list
                            (make-instance 'xml-element
                                           :name "body"
                                           :children (list
                                                      (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (content-of node)))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-list))
  (ecase (orientation-of node)
    (:vertical
     (make-instance 'xml-element
                    :name "table"
                    :children (mapcar (lambda (node)
                                        (make-instance 'xml-element
                                                       :name "tr"
                                                       :children (list
                                                                  (make-instance 'xml-element
                                                                                 :name "td"
                                                                                 :children (list
                                                                                            (transform-quasi-quoted-typesetting-to-quasi-quoted-xml node))))))
                                      (elements-of node))))
    (:horizontal
     (make-instance 'xml-element
                    :name "table"
                    :children (list
                               (make-instance 'xml-element
                                              :name "tr"
                                              :children (mapcar
                                                         (lambda (node)
                                                           (make-instance 'xml-element
                                                                          :name "td"
                                                                          :children (list
                                                                                     (transform-quasi-quoted-typesetting-to-quasi-quoted-xml node))))
                                                         (elements-of node))))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-paragraph))
  (make-instance 'xml-element
                 :name "span"
                 :children (mapcar (lambda (node)
                                     (make-instance 'xml-text :content (transform-quasi-quoted-typesetting-to-quasi-quoted-xml node)))
                                   (contents-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-menu))
  (make-instance 'xml-element
                 :name "ul"
                 :children (mapcar 'transform-quasi-quoted-typesetting-to-quasi-quoted-xml (menu-items-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-menu-item))
  (make-instance 'xml-element
                 :name "li"
                 :children (list
                            (make-instance 'xml-element
                                           :name "a"
                                           :attributes (list (make-instance 'xml-attribute
                                                                            :name "href"
                                                                            :value (make-instance 'xml-unquote
                                                                                                  :form `(registered-lambda ,(form-of (action-of node))))))
                                           :children (list
                                                      (make-instance 'xml-text
                                                                     :content (label-of node)))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-button))
  (make-instance 'xml-element
                 :name "a"
                 :attributes (list
                              (make-instance 'xml-attribute
                                             :name "href"
                                             :value (make-instance 'xml-unquote
                                                                   :form `(registered-lambda ,(form-of (action-of node))))))
                 :children (list
                            (make-instance 'xml-text
                                           :content (label-of node)))))

(def (special-variable e) *registered-lambdas* (make-hash-table :test #'equal))

(def macro registered-lambda (&body forms)
  `(bind ((key (symbol-name (gensym))))
     (setf (gethash key *registered-lambdas*)
           (lambda ()
             ,@forms))
     key))

