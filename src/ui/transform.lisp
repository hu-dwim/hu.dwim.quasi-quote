;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-ui)

;;;;;;;;;
;;; XHTML

(enable-quasi-quoted-xml-to-xml-emitting-form-syntax)

(def method transform ((to (eql 'quasi-quoted-xml)) (input ui-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-ui-to-quasi-quoted-xml input args))

(defgeneric transform-quasi-quoted-ui-to-quasi-quoted-xml (node))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node function))
  node)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node quasi-quote))
  (if (typep node 'xml-quasi-quote)
      (body-of node)
      node))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node unquote))
  (transform 'quasi-quoted-xml node))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node string))
  node)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node number))
  (princ-to-string node))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-quasi-quote))
  (make-xml-quasi-quote (transform-quasi-quoted-ui-to-quasi-quoted-xml (body-of node))))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-unquote))
  (make-xml-unquote
   ;; TODO: computed states should capture lexical variables and cache in hash-table like defcfun
   ;; `(transform-quasi-quoted-ui-to-quasi-quoted-xml (registered-component ,(form-of node)))))
   `(transform-quasi-quoted-ui-to-quasi-quoted-xml
     ,(map-filtered-tree (form-of node) 'ui-quasi-quote #'transform-quasi-quoted-ui-to-quasi-quoted-xml))))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-screen))
  <html
    <body
      ,(transform-quasi-quoted-ui-to-quasi-quoted-xml (content-of node))>>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-list))
  (ecase (orientation-of node)
    (:vertical
     <table
       ,@(mapcar
          (lambda (node)
            <tr
              <td
                ,(transform-quasi-quoted-ui-to-quasi-quoted-xml node)>>)
          (elements-of node))>)
    (:horizontal
     <table
       <tr
         ,@(mapcar
            (lambda (node)
              <td
                ,(transform-quasi-quoted-ui-to-quasi-quoted-xml node)>)
            (elements-of node))>>)))

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-text))
  <span ,@(mapcar (lambda (node)
                    (if (typep node 'ui-unquote)
                        (make-xml-unquote `(make-xml-text ,(form-of node)))
                        (make-xml-text node)))
                  (contents-of node))>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-menu))
  <ul ,@(mapcar 'transform-quasi-quoted-ui-to-quasi-quoted-xml (menu-items-of node))>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-content-menu))
  <table
    <tr
      <td
        <ul ,@(mapcar 'transform-quasi-quoted-ui-to-quasi-quoted-xml (menu-items-of node))>>
      <td ,(make-xml-unquote (with-unique-names (content)
                               `(bind ((,content ,(form-of (place-of node))))
                                  (if (functionp ,content)
                                      (funcall ,content)
                                      ,content))))>>>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-menu-item))
  <li
    <a (:href ,(make-xml-unquote `(registered-action ,(form-of (action-of node)))))
      ,(make-xml-text (label-of node))>>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-action))
  <a (:href ,(make-xml-unquote `(registered-action ,(form-of (action-of node)))))
    ,(bind ((label (label-of node)))
           (if (typep label 'ui-unquote)
               (make-xml-unquote `(make-xml-text (princ-to-string ,(form-of label))))
               (make-xml-text (transform-quasi-quoted-ui-to-quasi-quoted-xml label))))>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-text-field))
  <input (:type "text"
          :value ,(transform-quasi-quoted-ui-to-quasi-quoted-xml (place-of node))
          :name ,(make-xml-unquote `(registered-input ,(form-of (place-of node)))))>)

(def method transform-quasi-quoted-ui-to-quasi-quoted-xml ((node ui-form))
  <form ,(transform-quasi-quoted-ui-to-quasi-quoted-xml (content-of node))>)

;;;;;;;
;;; PDF

(enable-quasi-quoted-pdf-to-pdf-emitting-form-syntax)

#+nil ;; use in tests
(set-quasi-quoted-ui-syntax-in-readtable :transform `(quasi-quoted-pdf quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream-name *pdf-stream*)))

(def method transform ((to (eql 'quasi-quoted-pdf)) (input ui-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-ui-to-quasi-quoted-pdf input args))

(defgeneric transform-quasi-quoted-ui-to-quasi-quoted-pdf (node)
  (:method ((node function))
    node)

  (:method ((node quasi-quote))
    (if (typep node 'xml-quasi-quote)
        (body-of node)
        node))

  (:method ((node unquote))
    (transform 'quasi-quoted-xml node))

  (:method ((node ui-quasi-quote))
    (make-pdf-quasi-quote (transform-quasi-quoted-ui-to-quasi-quoted-pdf (body-of node))))

  (:method ((node ui-unquote))
    (make-pdf-unquote
     `(transform-quasi-quoted-ui-to-quasi-quoted-pdf
       ,(map-filtered-tree (form-of node) 'ui-quasi-quote #'transform-quasi-quoted-ui-to-quasi-quoted-pdf))))

  (:method ((node ui-screen))
    [document [info [dictionary "Author" "levy"]]
              [root [catalog "Pages"
                             [pages "Count" 0
                                    "MediaBox" [array 0 0 612 792]]]]]))
