;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-ui)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-ui (&key (start-character #\[)
                                     (end-character #\])
                                     (unquote-character #\,)
                                     (splice-character #\@)
                                     (transformation nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (readtime-chain-transform transformation (make-ui-quasi-quote (parse-ui-reader-body body))))
   (lambda (form spliced)
     (make-ui-unquote form spliced))
   :nested-quasi-quote-wrapper (lambda (body)
                                 (parse-ui-reader-body body))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-ui-to-xml-string ()
  (set-quasi-quoted-ui-syntax-in-readtable :transformation '(quasi-quoted-xml quasi-quoted-string string)))

(define-syntax quasi-quoted-ui-to-xml-string-emitting-form ()
  (set-quasi-quoted-ui-syntax-in-readtable :transformation '(quasi-quoted-xml quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-ui-to-xml-binary ()
  (set-quasi-quoted-ui-syntax-in-readtable :transformation '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary)))

(define-syntax quasi-quoted-ui-to-xml-binary-emitting-form ()
  (set-quasi-quoted-ui-syntax-in-readtable :transformation '(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-ui-to-xml-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-ui-syntax-in-readtable :transformation `(quasi-quoted-xml quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(define-syntax quasi-quoted-ui-to-pdf-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-ui-syntax-in-readtable :transformation `(quasi-quoted-pdf quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(def function ui-syntax-node-name (name)
  (format-symbol (find-package :cl-quasi-quote-ui) "UI-~A" name))

(def function parse-ui-reader-body (form)
  (if (typep form 'syntax-node)
      form
      (bind ((sexp-parser (gethash (first form) *ui-ast-node-name->sexp-parser*)))
        (assert sexp-parser)
        (funcall sexp-parser form))))

;; TODO this is bullshit from here, create some specification language for the ui-ast-node definer that describes how to parse the sexps

(def ui-ast-node-parser screen
  (make-instance 'ui-screen
                 :content (second -sexp-)))

(def ui-ast-node-parser content-menu
    (make-instance 'ui-content-menu
                   :place (second -sexp-)
                   :menu-items (cddr -sexp-)))

(def ui-ast-node-parser menu-item
  (make-instance 'ui-menu-item
                 :label (second -sexp-)
                 :action (third -sexp-)))

(def ui-ast-node-parser action
  (make-instance 'ui-action
                 :label (second -sexp-)
                 :action (third -sexp-)))

(def ui-ast-node-parser vertical-list
  (make-instance 'ui-vertical-list
                 :elements (rest -sexp-)))

(def ui-ast-node-parser text
  (make-instance 'ui-text
                 :contents (rest -sexp-)))

#|

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

(def method parse-quasi-quoted-ui* ((first (eql 'ui-table)) whole)
  (make-instance 'ui-table
                 :rows (mapcar #'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-row)) whole)
  (make-instance 'ui-row
                 :cells (mapcar #'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'ui-cell)) whole)
  (make-instance 'ui-cell
                 :content (parse-quasi-quoted-ui (second whole))))

|#
