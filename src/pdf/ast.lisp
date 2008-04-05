;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-pdf)

(def function pdf-syntax-node-name (name)
  (format-symbol #.(find-package :cl-quasi-quote-pdf) "PDF-~A" name))

;;;;;;;
;;; AST

(def ast pdf)

(def special-variable *compile-time-pdf-node-identity-counter* 0
  "This is a monotonically increasing integer which is, at runtime, used to represent the identity of AST nodes that were created and thrown away at compile time.")

(def class* pdf-syntax-node (syntax-node parent-mixin)
  ())

(def (class* e) pdf-quasi-quote (quasi-quote pdf-syntax-node)
  ())

(def (function e) make-pdf-quasi-quote (body)
  (make-instance 'pdf-quasi-quote :body body))

(def (class* e) pdf-unquote (unquote pdf-syntax-node)
  ())

(def (function e) make-pdf-unquote (form &optional (spliced? #f))
  (make-instance 'pdf-unquote :form form :spliced spliced?))

(def (class*) pdf-object-identifier (pdf-syntax-node)
  ;; TODO atomic-incf
  ((node-identity (incf *compile-time-pdf-node-identity-counter*) :type integer)
   (object-id nil :type integer)
   (generation-number 0 :type integer)))

(def (class*) pdf-indirect-object (pdf-object-identifier)
  ((name :type symbol)
   (content :type pdf-syntax-node)))

(def (class*) pdf-indirect-object-reference (pdf-object-identifier)
  ((name nil :type symbol)))

(def (class*) pdf-null (pdf-syntax-node)
  ())

(def (class*) pdf-boolean (pdf-syntax-node)
  ((value :type boolean)))

(def (class*) pdf-number (pdf-syntax-node)
  ((value :type number)))

(def (class*) pdf-string (pdf-syntax-node)
  ((value :type string)))

(def (class*) pdf-name (pdf-syntax-node)
  ((value :type string)))

(def (class*) pdf-array (pdf-syntax-node)
  ((value :type list)))

(def (class*) pdf-stream (pdf-syntax-node)
  ((contents :type list)))

(def (class*) pdf-begin-text (pdf-syntax-node)
  ())

(def (class*) pdf-end-text (pdf-syntax-node)
  ())

(def (class*) pdf-set-font (pdf-syntax-node)
  ((name :type string)
   (size :type float)))

;; TODO: this is actually move text
(def (class*) pdf-move-text (pdf-syntax-node)
  ((x :type float)
   (y :type float)))

(def (class*) pdf-display-text (pdf-syntax-node)
  ())

(def (class*) pdf-dictionary (pdf-syntax-node)
  ((map (make-hash-table) :type hash-table)))

(def (class*) pdf-catalog (pdf-dictionary)
  ())

(def (class*) pdf-pages (pdf-dictionary)
  ())

(def (class*) pdf-page (pdf-dictionary)
  ())

(def (class*) pdf-root (pdf-indirect-object)
  ((content)))

(def (class*) pdf-info (pdf-indirect-object)
  ((content)))

(def (class*) pdf-position-mixin ()
  ((position 0 :type integer)))

(def (class*) pdf-xref-entry (pdf-object-identifier pdf-position-mixin)
  ((free :type boolean)))

(def (class*) pdf-xref-section (pdf-syntax-node)
  ((entries nil :type list)))

(def (class*) pdf-xref (pdf-syntax-node pdf-position-mixin)
  ((sections nil :type list)))

(def (class*) pdf-header (pdf-syntax-node)
  ((version "1.4" :type string)))

(def (class*) pdf-trailer (pdf-syntax-node)
  ((dictionary (make-trailer-dictionary) :type pdf-dictionary)))

(def function make-trailer-dictionary ()
  (bind ((map (make-hash-table)))
    (setf (gethash (make-instance 'pdf-name :value "Root") map)
          (make-pdf-unquote '(root-reference-of *pdf-environment*)))
    (setf (gethash (make-instance 'pdf-name :value "Info") map)
          (make-pdf-unquote '(info-reference-of *pdf-environment*)))
    (setf (gethash (make-instance 'pdf-name :value "Size") map)
          (make-bivalent-unquote '(princ-to-string (compute-xref-size (xref-of *pdf-environment*)))))
    (make-instance 'pdf-dictionary :map map)))

(def (class*) pdf-document (pdf-syntax-node)
  ((header (make-instance 'pdf-header) :type pdf-header)
   (trailer (make-instance 'pdf-trailer) :type pdf-trailer)
   (elements :type list)
   (xref (make-instance 'pdf-xref
                        :sections (list (make-instance 'pdf-xref-section
                                                       :entries (list (make-instance 'pdf-xref-entry
                                                                                     :free #t
                                                                                     :object-id 0
                                                                                     :generation-number 65535)))))
         :type pdf-xref)))
