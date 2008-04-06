;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

;; TODO: separate package?
(in-package :cl-quasi-quote)

(enable-quasi-quoted-xml-to-xml-emitting-form-syntax)

;;;;;;;;;
;;; Parse

(def special-variable *quasi-quoted-typesetting-nesting-level*)

(define-syntax quasi-quoted-typesetting (&key (quasi-quote-character #\[)
                                              (quasi-quote-end-character #\])
                                              (unquote-character #\,)
                                              (splice-character #\@)
                                              (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-typesetting-quasi-quote (parse-quasi-quoted-typesetting body)))))
   (lambda (form spliced)
     (make-typesetting-unquote form spliced))
   '*quasi-quoted-typesetting-nesting-level*
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-typesetting-to-string ()
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string string)))

(define-syntax quasi-quoted-typesetting-to-string-emitting-form ()
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-typesetting-to-binary ()
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary)))

(define-syntax quasi-quoted-typesetting-to-binary-emitting-form ()
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-typesetting-to-binary-stream-emitting-form (stream)
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform `(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream ,stream))))

(def function typesetting-syntax-node-name (name)
  (format-symbol (find-package :cl-quasi-quote) "TYPESETTING-~A" name))

(def function parse-quasi-quoted-typesetting (form)
  (if (typep form 'syntax-node)
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

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-text)) whole)
  (make-instance 'typesetting-text
                 :contents (cdr whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-menu)) whole)
  (make-instance 'typesetting-menu
                 :menu-items (mapcar 'parse-quasi-quoted-typesetting (cdr whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-content-menu)) whole)
  (make-instance 'typesetting-content-menu
                 :place (second whole)
                 :menu-items (mapcar 'parse-quasi-quoted-typesetting (cddr whole))))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-menu-item)) whole)
  (make-instance 'typesetting-menu-item
                 :label (second whole)
                 :action (third whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-action)) whole)
  (make-instance 'typesetting-action
                 :label (second whole)
                 :action (third whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-text-field)) whole)
  (make-instance 'typesetting-text-field
                 :place (second whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-form)) whole)
  (make-instance 'typesetting-form
                 :content (parse-quasi-quoted-typesetting (second whole))))

;;;;;;;
;;; AST

(def ast typesetting)

(def class* typesetting-syntax-node (syntax-node)
  ())

(def class* typesetting-quasi-quote (quasi-quote typesetting-syntax-node)
  ())

(def (function e) make-typesetting-quasi-quote (body)
  (make-instance 'typesetting-quasi-quote :body body))

(def class* typesetting-unquote (unquote typesetting-syntax-node)
  ())

(def (function e) make-typesetting-unquote (form &optional (spliced? #f))
  (make-instance 'typesetting-unquote :form form :spliced spliced?))

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

(def (class* e) typesetting-text (typesetting-syntax-node)
  ((contents)))

(def (class* e) typesetting-menu (typesetting-syntax-node)
  ((menu-items)))

(def (class* e) typesetting-content-menu (typesetting-menu typesetting-list)
  ((place)))

(def (class* e) typesetting-menu-item (typesetting-syntax-node)
  ((label)
   (action)))

(def (class* e) typesetting-action (typesetting-syntax-node)
  ((label)
   (action)))

(def (class* e) typesetting-text-field (typesetting-syntax-node)
  ((place)))

(def (class* e) typesetting-form (typesetting-syntax-node)
  ((content)))

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'quasi-quoted-xml)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-typesetting-to-quasi-quoted-xml input args))

(defgeneric transform-quasi-quoted-typesetting-to-quasi-quoted-xml (node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node function))
  node)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node quasi-quote))
  (if (typep node 'xml-quasi-quote)
      (body-of node)
      node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node unquote))
  (transform 'quasi-quoted-xml node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node string))
  node)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node number))
  (princ-to-string node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-quasi-quote))
  (make-xml-quasi-quote (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (body-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-unquote))
  (make-xml-unquote
   ;; TODO: computed states should capture lexical variables and cache in hash-table like defcfun
   ;; `(transform-quasi-quoted-typesetting-to-quasi-quoted-xml (registered-component ,(form-of node)))))
   `(transform-quasi-quoted-typesetting-to-quasi-quoted-xml
     ,(map-filtered-tree (form-of node) 'typesetting-quasi-quote #'transform-quasi-quoted-typesetting-to-quasi-quoted-xml))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-screen))
  <html
    <body
      ,(transform-quasi-quoted-typesetting-to-quasi-quoted-xml (content-of node))>>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-list))
  (ecase (orientation-of node)
    (:vertical
     <table
       ,@(mapcar
          (lambda (node)
            <tr
              <td
                ,(transform-quasi-quoted-typesetting-to-quasi-quoted-xml node)>>)
          (elements-of node))>)
    (:horizontal
     <table
       <tr
         ,@(mapcar
            (lambda (node)
              <td
                ,(transform-quasi-quoted-typesetting-to-quasi-quoted-xml node)>)
            (elements-of node))>>)))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-text))
  <span ,@(mapcar (lambda (node)
                    (if (typep node 'typesetting-unquote)
                        (make-xml-unquote `(make-xml-text ,(form-of node)))
                        (make-xml-text node)))
                  (contents-of node))>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-menu))
  <ul ,@(mapcar 'transform-quasi-quoted-typesetting-to-quasi-quoted-xml (menu-items-of node))>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-content-menu))
  <table
    <tr
      <td
        <ul ,@(mapcar 'transform-quasi-quoted-typesetting-to-quasi-quoted-xml (menu-items-of node))>>
      <td ,(make-xml-unquote (with-unique-names (content)
                               `(bind ((,content ,(form-of (place-of node))))
                                  (if (functionp ,content)
                                      (funcall ,content)
                                      ,content))))>>>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-menu-item))
  <li
    <a (:href ,(make-xml-unquote `(registered-action ,(form-of (action-of node)))))
      ,(make-xml-text (label-of node))>>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-action))
  <a (:href ,(make-xml-unquote `(registered-action ,(form-of (action-of node)))))
    ,(bind ((label (label-of node)))
           (if (typep label 'typesetting-unquote)
               (make-xml-unquote `(make-xml-text (princ-to-string ,(form-of label))))
               (make-xml-text (transform-quasi-quoted-typesetting-to-quasi-quoted-xml label))))>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-text-field))
  <input (:type "text"
          :value ,(transform-quasi-quoted-typesetting-to-quasi-quoted-xml (place-of node))
          :name ,(make-xml-unquote `(registered-input ,(form-of (place-of node)))))>)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-form))
  <form ,(transform-quasi-quoted-typesetting-to-quasi-quoted-xml (content-of node))>)

#||

;; TODO: the following parts are experimental and should be deleted
#.(use-package :computed-class)

(define-computed-universe compute-as)
(export 'compute-as)

(def (special-variable e) *registered-components* (make-hash-table :test #'eql))

(def (special-variable e) *registered-actions* (make-hash-table :test #'eql))

(def (special-variable e) *registered-parsers* (make-hash-table :test #'eql))

(def macro registered-action (&body forms)
  `(bind ((key (random 10000000000)))
     (setf (gethash key *registered-actions*)
           (lambda () ,@forms))
     (merge-pathnames (princ-to-string key)
                      (ucw::query-path (ucw:context.request ucw:*context*)))))

(def macro registered-component (&body forms)
  `(bind ((key (random 10000000000))
          ((:values computed-state found?)
           (gethash key *registered-components*
                    (compute-as* (:kind cc::standalone) ,@forms))))
     (unless found?
       (setf (gethash key *registered-components*) computed-state))
     (computed-state-value computed-state)))

(def macro registered-input (place)
  `(bind ((key (random 10000000000)))
     (setf (gethash key *registered-parsers*)
           ;; TODO: parse value from HTTP request
           (lambda () (setf ,place nil)))
     (merge-pathnames (princ-to-string key)
                      (ucw::query-path (ucw:context.request ucw:*context*)))))

(def (function e) handle-request (screen-thunk)
  (write-string
   (bind ((path (ucw::query-path (ucw:context.request ucw:*context*)))
          (name (pathname-name path))
          (key (when name (parse-integer name  :junk-allowed #t)))
          (handler (gethash key *registered-actions*)))
     (when handler
       (funcall handler))
     (maphash-values (lambda (value)
                       (unless (cc::computed-state-valid-p value)
                         ;; TODO: sort and find out which computed state should be sent to the client
                         value))
                     *registered-components*)
     (body-of (funcall screen-thunk)))
   (ucw:html-stream (ucw:context.response ucw:*context*))))

||#
