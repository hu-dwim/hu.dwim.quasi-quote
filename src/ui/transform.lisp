;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-ui)

;;;;;;;;;
;;; XHTML

(enable-quasi-quoted-xml-to-xml-emitting-form-syntax)

(defgeneric transform-quasi-quoted-ui-to-quasi-quoted-xml (node)
  (:method ((node function))
    node)

  (:method ((node quasi-quote))
    (if (typep node 'xml-quasi-quote)
        (body-of node)
        node))

  (:method ((node unquote))
    (transform 'quasi-quoted-xml node))

  (:method ((node string))
    node)

  (:method ((node number))
    (princ-to-string node))

  (:method ((node ui-quasi-quote))
    (make-xml-quasi-quote (transform-quasi-quoted-ui-to-quasi-quoted-xml (body-of node))))

  (:method ((node ui-unquote))
    (make-xml-unquote
     ;; TODO: computed states should capture lexical variables and cache in hash-table like defcfun
     ;; `(transform-quasi-quoted-ui-to-quasi-quoted-xml (registered-component ,(form-of node)))))
     `(transform-quasi-quoted-ui-to-quasi-quoted-xml
       ,(map-filtered-tree (form-of node) 'ui-quasi-quote #'transform-quasi-quoted-ui-to-quasi-quoted-xml))))

  (:method ((node ui-screen))
    <html
     <body
      ,(transform-quasi-quoted-ui-to-quasi-quoted-xml (content-of node))>>)

  (:method ((node ui-list))
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

  (:method ((node ui-text))
    <span ,@(mapcar (lambda (node)
                      (if (typep node 'ui-unquote)
                          (make-xml-unquote `(make-xml-text ,(form-of node)))
                          (make-xml-text node)))
                    (contents-of node))>)

  (:method ((node ui-menu))
    <ul ,@(mapcar 'transform-quasi-quoted-ui-to-quasi-quoted-xml (menu-items-of node))>)

  (:method ((node ui-content-menu))
    <table
        <tr
         <td
          <ul ,@(mapcar 'transform-quasi-quoted-ui-to-quasi-quoted-xml (menu-items-of node))>>
         <td ,(make-xml-unquote (with-unique-names (content)
                                  `(bind ((,content ,(form-of (place-of node))))
                                     (if (functionp ,content)
                                         (funcall ,content)
                                         ,content))))>>>)

  (:method ((node ui-menu-item))
    <li
     <a (:href ,(make-xml-unquote `(registered-action ,(form-of (action-of node)))))
        ,(make-xml-text (label-of node))>>)

  (:method ((node ui-action))
    <a (:href ,(make-xml-unquote `(registered-action ,(form-of (action-of node)))))
       ,(bind ((label (label-of node)))
              (if (typep label 'ui-unquote)
                  (make-xml-unquote `(make-xml-text (princ-to-string ,(form-of label))))
                  (make-xml-text (transform-quasi-quoted-ui-to-quasi-quoted-xml label))))>)

  (:method ((node ui-text-field))
    <input (:type "text"
                  :value ,(transform-quasi-quoted-ui-to-quasi-quoted-xml (place-of node))
                  :name ,(make-xml-unquote `(registered-input ,(form-of (place-of node)))))>)

  (:method ((node ui-form))
    <form ,(transform-quasi-quoted-ui-to-quasi-quoted-xml (content-of node))>))

(def method transform ((to (eql 'quasi-quoted-xml)) (input ui-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-ui-to-quasi-quoted-xml input args))

;;;;;;;
;;; PDF

(enable-quasi-quoted-pdf-to-pdf-emitting-form-syntax)

(defgeneric transform-quasi-quoted-ui-to-quasi-quoted-pdf (node)
  (:method ((node function))
    node)

  (:method ((node string))
    [string ,node])

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
    [document [info [dictionary "Generated" #t]]
              [root [catalog "Pages" [indirect-object-reference pages]]]
              [indirect-object pages
                               [pages "Count" 1
                                      "Kids" [array [indirect-object-reference page1]]
                                      "MediaBox" [array 0 0 612 792]]]
              [indirect-object page1
                               [page "Parent" [indirect-object-reference pages]
                                     "Resources" [dictionary
                                                  "ProcSet" [array [name "PDF"] [name "Text"]]
                                                  "Font" [dictionary
                                                          "F1" [dictionary
                                                                "Type"     [name "Font"]
                                                                "Subtype"  [name "Type1"]
                                                                "Name"     [name "F1"]
                                                                "BaseFont" [name "Times-Roman"]]]]
                                     "Contents" [indirect-object-reference content]]]
              [indirect-object content ,(transform-quasi-quoted-ui-to-quasi-quoted-pdf (content-of node))]])

  (:method ((node ui-list))
    ;; TODO:
    [null])

  (:method ((node ui-text))
    ;; TODO:
    [stream
     [begin-text]
     [set-font "F1" 12]
     [move-text 72 712]
     ,@(mapcar 'transform-quasi-quoted-ui-to-quasi-quoted-pdf (contents-of node))
     [display-text]
     [end-text]]))

(def method transform ((to (eql 'quasi-quoted-pdf)) (input ui-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-ui-to-quasi-quoted-pdf input args))
