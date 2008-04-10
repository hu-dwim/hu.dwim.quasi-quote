;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-ui)

;;;;;;;;;
;;; Parse

(def special-variable *quasi-quoted-ui-nesting-level*)

(define-syntax quasi-quoted-ui (&key (start-character #\[)
                                     (end-character #\])
                                     (unquote-character #\,)
                                     (splice-character #\@)
                                     (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((cl-quasi-quote::*quasi-quote-level* (1+ cl-quasi-quote::*quasi-quote-level*)))
       (readtime-chain-transform transform (make-ui-quasi-quote (parse-quasi-quoted-ui body)))))
   (lambda (form spliced)
     (make-ui-unquote form spliced))
   '*quasi-quoted-ui-nesting-level*
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-ui-to-xml-string ()
  (set-quasi-quoted-ui-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string string)))

(define-syntax quasi-quoted-ui-to-xml-string-emitting-form ()
  (set-quasi-quoted-ui-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-ui-to-xml-binary ()
  (set-quasi-quoted-ui-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary)))

(define-syntax quasi-quoted-ui-to-xml-binary-emitting-form ()
  (set-quasi-quoted-ui-syntax-in-readtable :transform '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-ui-to-xml-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-ui-syntax-in-readtable :transform `(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(define-syntax quasi-quoted-ui-to-pdf-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-ui-syntax-in-readtable :transform `(quasi-quoted-pdf quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(def function ui-syntax-node-name (name)
  (format-symbol (find-package :cl-quasi-quote-ui) "UI-~A" name))

(def function parse-quasi-quoted-ui (form)
  (if (typep form 'syntax-node)
      form
      (parse-quasi-quoted-ui* (ui-syntax-node-name (first form)) form)))

(defgeneric parse-quasi-quoted-ui* (first whole))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-screen)) whole)
  (make-instance 'ui-screen
                 :content (parse-quasi-quoted-ui (second whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-list)) whole)
  (make-instance 'ui-list
                 :elements (mapcar 'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-vertical-list)) whole)
  (make-instance 'ui-vertical-list
                 :elements (mapcar 'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-horizontal-list)) whole)
  (make-instance 'ui-horizontal-list
                 :elements (mapcar 'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-text)) whole)
  (make-instance 'ui-text
                 :contents (cdr whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-menu)) whole)
  (make-instance 'ui-menu
                 :menu-items (mapcar 'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-content-menu)) whole)
  (make-instance 'ui-content-menu
                 :place (second whole)
                 :menu-items (mapcar 'parse-quasi-quoted-ui (cddr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-menu-item)) whole)
  (make-instance 'ui-menu-item
                 :label (second whole)
                 :action (third whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-action)) whole)
  (make-instance 'ui-action
                 :label (second whole)
                 :action (third whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-text-field)) whole)
  (make-instance 'ui-text-field
                 :place (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-form)) whole)
  (make-instance 'ui-form
                 :content (parse-quasi-quoted-ui (second whole))))
