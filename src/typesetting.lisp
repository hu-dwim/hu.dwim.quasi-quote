;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

;; TODO: separate package?
(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-typesetting (&key (quasi-quote-character #\[)
                                              (quasi-quote-end-character #\])
                                              (unquote-character #\,)
                                              (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (make-instance 'typesetting-quasi-quote :body (map-tree body
                                                             (lambda (form)
                                                               (if (symbolp form)
                                                                   (make-instance form)
                                                                   form)))))
   (lambda (form spliced)
     (make-instance 'typesetting-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

;;;;;;;
;;; AST

(def ast typesetting)

(def class* typesetting-syntax-node (syntax-node)
  ())

(def class* typesetting-quasi-quote (quasi-quote typesetting-syntax-node)
  ())

(def class* typesetting-unquote (unquote typesetting-syntax-node)
  ())

(def (class* e) typesetting-list (typesetting-syntax-node)
  ((orientation :vertical :type (member :horizontal :vertical))))

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'string)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string (transform 'quasi-quoted-xml input) args))

(def method transform ((to (eql 'string-emitting-form)) (input typesetting-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string-emitting-form (transform 'quasi-quoted-xml input) args))

(def method transform ((to (eql 'quasi-quoted-xml)) (input typesetting-syntax-node) &key &allow-other-keys)
  (etypecase input
    (typesetting-quasi-quote
     (make-instance 'xml-quasi-quote
                    :body (map-tree (body-of input)
                                    (lambda (node)
                                      ;; TODO: generice method
                                      (etypecase node
                                        (typesetting-list
                                         (make-instance 'xml-element
                                                        :name "div")))))))
    (typesetting-unquote
     (break))))
