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
                                              (splice-character #\@)
                                              (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (chain-transform transform
                      (make-instance 'typesetting-quasi-quote :body (parse-quasi-quoted-typesetting body))))
   (lambda (form spliced)
     (make-instance 'typesetting-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-typesetting-to-string ()
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-typesetting-to-binary ()
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-typesetting-to-binary-stream (stream)
  (set-quasi-quoted-typesetting-syntax-in-readtable :transform `(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream ,stream))))

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

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-text)) whole)
  (make-instance 'typesetting-text
                 :contents (cdr whole)))

(def method parse-quasi-quoted-typesetting* ((first (eql 'typesetting-paragraph)) whole)
  (make-instance 'typesetting-paragraph
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

(def (class* e) typesetting-text (typesetting-syntax-node)
  ((contents)))

(def (class* e) typesetting-paragraph (typesetting-syntax-node)
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

(def method transform ((to (eql 'string)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string (transform 'quasi-quoted-xml input) args))

(def method transform ((to (eql 'string-emitting-form)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string-emitting-form (transform 'quasi-quoted-xml input) args))

(def method transform ((to (eql 'quasi-quoted-xml)) (input typesetting-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-typesetting-to-quasi-quoted-xml input))

(defgeneric transform-quasi-quoted-typesetting-to-quasi-quoted-xml (node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node function))
  node)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node string))
  node)

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node number))
  (princ-to-string node))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-quasi-quote))
  (make-instance 'xml-quasi-quote
                 :body (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (body-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-unquote))
  (make-instance 'xml-unquote
                 ;; TODO: computed states should capture lexical variables and cache in hash-table like defcfun
;;                 :form `(transform-quasi-quoted-typesetting-to-quasi-quoted-xml (registered-component ,(form-of node)))))
                 :form `(transform-quasi-quoted-typesetting-to-quasi-quoted-xml ,(form-of node))))

;; TODO: kill superfluous ()
(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-screen))
  {(with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'xml-emitting-form)
   <html
     <body
       ,@(list (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (content-of node)))>>})

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

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-text))
  (make-instance 'xml-element
                 :name "span"
                 :children (mapcar (lambda (node)
                                     (make-instance 'xml-text
                                                    :content (transform-quasi-quoted-typesetting-to-quasi-quoted-xml node)))
                                   (contents-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-paragraph))
  (make-instance 'xml-element
                 :name "span"
                 :children (mapcar (lambda (node)
                                     (make-instance 'xml-text
                                                    :content (transform-quasi-quoted-typesetting-to-quasi-quoted-xml node)))
                                   (contents-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-menu))
  (make-instance 'xml-element
                 :name "ul"
                 :children (mapcar 'transform-quasi-quoted-typesetting-to-quasi-quoted-xml (menu-items-of node))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-content-menu))
  (make-instance 'xml-element
                 :name "table"
                 :children (list
                            (make-instance 'xml-element
                                           :name "tr"
                                           :children (list
                                                      (make-instance 'xml-element
                                                                     :name "td"
                                                                     :children (list
                                                                                (make-instance 'xml-element
                                                                                               :name "ul"
                                                                                               :children (mapcar 'transform-quasi-quoted-typesetting-to-quasi-quoted-xml (menu-items-of node)))))
                                                      (make-instance 'xml-element
                                                                     :name "td"
                                                                     :children (list
                                                                                (make-instance 'xml-unquote
                                                                                               :form (with-unique-names (content)
                                                                                                       `(bind ((,content ,(form-of (place-of node))))
                                                                                                          (if (functionp ,content)
                                                                                                              (funcall ,content)
                                                                                                              ,content)))))))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-menu-item))
  (make-instance 'xml-element
                 :name "li"
                 :children (list
                            (make-instance 'xml-element
                                           :name "a"
                                           :attributes (list (make-instance 'xml-attribute
                                                                            :name "href"
                                                                            :value (make-instance 'xml-unquote
                                                                                                  :form `(registered-action ,(form-of (action-of node))))))
                                           :children (list
                                                      (make-instance 'xml-text
                                                                     :content (label-of node)))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-action))
  (make-instance 'xml-element
                 :name "a"
                 :attributes (list
                              (make-instance 'xml-attribute
                                             :name "href"
                                             :value (make-instance 'xml-unquote
                                                                   :form `(registered-action ,(form-of (action-of node))))))
                 :children (list
                            (make-instance 'xml-text
                                           :content (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (label-of node))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-text-field))
  (make-instance 'xml-element
                 :name "input"
                 :attributes (list
                              (make-instance 'xml-attribute
                                             :name "name"
                                             :value (make-instance 'xml-unquote :form `(registered-input ,(form-of (place-of node)))))
                              (make-instance 'xml-attribute
                                             :name "type"
                                             :value "text")
                              (make-instance 'xml-attribute
                                             :name "value"
                                             :value (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (place-of node))))))

(def method transform-quasi-quoted-typesetting-to-quasi-quoted-xml ((node typesetting-form))
  (make-instance 'xml-element
                 :name "form"
                 :children (list
                            (transform-quasi-quoted-typesetting-to-quasi-quoted-xml (content-of node)))))

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
     (funcall screen-thunk))
   (ucw:html-stream (ucw:context.response ucw:*context*))))
