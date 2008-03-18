;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;; TODO: separate package?
(def class* typesetting-syntax-node (syntax-node)
  ())

(def (class* e) typesetting-list (typesetting-syntax-node)
  ((orientation :vertical :type (member :horizontal :vertical))))

(def (function e) transform-quasi-quoted-typesetting-to-quasi-quoted-xhtml (qq-typesetting)
  (etypecase qq-typesetting
    (quasi-quote
     (labels ((process (node)
                ;; TODO: generice method
                (etypecase node
                  (typesetting-list
                   (make-instance 'xml-element
                                  :name "div")))))
       (make-instance 'quasi-quote
                      :body (bind ((body (body-of qq-typesetting)))
                              (if (consp body)
                                  (mapcar #'process body)
                                  (list (process body)))))))
    (unquote
     (break))))

(def (function e) transform-quasi-quoted-typesetting-to-xhtml-string (qq-typesetting)
  (transform-quasi-quoted-string-to-string
   (transform-quasi-quoted-xml-to-quasi-quoted-string
    (transform-quasi-quoted-typesetting-to-quasi-quoted-xhtml qq-typesetting))))

(def (function e) expand-quasi-quoted-typesetting-to-lambda-form (qq-typesetting)
  (expand-quasi-quoted-string-to-lambda-form
   (transform-quasi-quoted-xml-to-quasi-quoted-string
    (transform-quasi-quoted-typesetting-to-quasi-quoted-xhtml qq-typesetting))))
