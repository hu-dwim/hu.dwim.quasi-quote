;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-pdf)

;;;;;;;;;
;;; Parse
;;;
(def special-variable *pdf-quasi-quote-level* 0)

(define-syntax quasi-quoted-pdf (&key (start-character #\[)
                                      (end-character #\])
                                      (unquote-character #\,)
                                      (splice-character #\@)
                                      (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (readtime-chain-transform transform (make-pdf-quasi-quote (parse-pdf-reader-body body))))
   (lambda (body spliced?)
     (make-pdf-unquote body spliced?))
   '*quasi-quoted-xml-nesting-level*
   :nested-quasi-quote-wrapper (lambda (body)
                                 (parse-pdf-reader-body body))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   ;;:readtable-case :preserve
   ))

(define-syntax quasi-quoted-pdf-to-pdf-emitting-form ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(pdf-emitting-form)))

(define-syntax quasi-quoted-pdf-to-binary ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary)))

(define-syntax quasi-quoted-pdf-to-binary-emitting-form ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-pdf-to-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-pdf-syntax-in-readtable :transform `(quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(def function parse-pdf-reader-body (form)
  (if (typep form 'syntax-node)
      form
      (bind ((sexp-parser (gethash (first form) *pdf-ast-node-name->sexp-parser*)))
        (assert sexp-parser)
        (funcall sexp-parser form))))

;;;
;;; Override some parsers where the default expansion from the pdf-ast-node definer is not ok
;;;
(def pdf-ast-node-parser document
  (make-pdf-document (rest -sexp-)))

(def pdf-ast-node-parser array
  (make-instance 'pdf-array :value (mapcar #'parse-into-pdf-syntax-node (rest -sexp-))))

(def pdf-ast-node-parser dictionary
  (parse-dictionary-map (make-instance 'pdf-dictionary) (rest -sexp-)))

(def pdf-ast-node-parser catalog
  (parse-dictionary-map (make-instance 'pdf-catalog) (rest -sexp-)))

(def pdf-ast-node-parser pages
  (parse-dictionary-map (make-instance 'pdf-pages) (rest -sexp-)))

(def pdf-ast-node-parser page
  (parse-dictionary-map (make-instance 'pdf-page) (rest -sexp-)))

(def pdf-ast-node-parser stream
  (make-instance 'pdf-stream :contents (rest -sexp-)))

(def function parse-dictionary-map (dictionary elements)
  (iter
    (with map = (map-of dictionary))
    (while elements)
    (for key = (pop elements))
    (for value = (pop elements))
    (setf key (etypecase key
                (pdf-name key)
                (string (make-pdf-name key))
                (symbol (make-pdf-name key))))
    (setf value (parse-into-pdf-syntax-node value))
    (setf (parent-of value) dictionary)
    (setf (parent-of key) dictionary)
    (setf (gethash key map) value))
  dictionary)

(def function parse-into-pdf-syntax-node (value)
  (if (eq value #t)
      (make-pdf-boolean #t)
      (etypecase value
        (pdf-syntax-node value)
        ((or integer float) (make-pdf-number value))
        (string (make-pdf-string value))
        (null (make-pdf-boolean #f))
        (vector (make-instance 'pdf-array :value (map 'list #'parse-into-pdf-syntax-node value))))))
