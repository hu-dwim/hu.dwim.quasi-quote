;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-pdf)

;;;;;;;
;;; AST

(def ast pdf)

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

(def special-variable *compile-time-pdf-node-identity-counter* 0
  "This is a monotonically increasing integer which is, at runtime, used to represent the identity of AST nodes that were created and thrown away at compile time.")

(def special-variable *pdf-ast-node-name->sexp-parser* (make-hash-table :test #'eq))

(def definer pdf-ast-node-parser (name &body body)
  `(setf (gethash ',name *pdf-ast-node-name->sexp-parser*)
         (lambda (-sexp-)
           (declare (ignorable -sexp-))
           (block nil
             ,@body))))

(def definer pdf-ast-node (name supers slots)
  (bind ((class-name (format-symbol #.(find-package :cl-quasi-quote-pdf) "PDF-~A" name))
         (defclass-form `(def class* ,class-name ,(or supers '(pdf-syntax-node)) ,slots))
         (expanded-slots (fourth (macroexpand-1 defclass-form)))
         (slot-accessors (mapcar (lambda (slot-definition)
                                   (getf (rest slot-definition) :accessor))
                                 expanded-slots)))
    `(progn
       ,defclass-form
       (def pdf-ast-node-parser ,name
         (bind ((result (make-instance ',class-name)))
           (pop -sexp-) ; pop the ast node name
           ,@(loop
                :for accessor :in slot-accessors
                :collect `(if (null -sexp-)
                              (return result)
                              (bind ((child (pop -sexp-)))
                                (setf (,accessor result) child)
                                ;; KLUDGE instead of a MOP extension, we maintain the parent chain like this...
                                (when (typep child 'parent-mixin)
                                  (setf (parent-of child) result)))))
           result))
       (export ',name *package*))))

(def pdf-ast-node object-identifier (pdf-syntax-node)
  ;; TODO atomic-incf
  ((node-identity (incf *compile-time-pdf-node-identity-counter*) :type integer)
   (object-id nil :type integer)
   (generation-number 0 :type integer)))

(def pdf-ast-node indirect-object (pdf-object-identifier)
  ((name :type symbol)
   (content :type pdf-syntax-node)))

(def pdf-ast-node indirect-object-reference (pdf-object-identifier)
  ((name nil :type symbol)))

(def pdf-ast-node null ()
  ())

(def pdf-ast-node boolean ()
  ((value :type boolean)))

(def function make-pdf-boolean (value)
  (make-instance 'pdf-boolean :value value))

(def pdf-ast-node number ()
  ((value :type number)))

(def constructor (pdf-number value)
  (when value
    (assert (satisfies-pdf-number-constraints? value) () "~S is not a valid pdf number" value)))

(def function make-pdf-number (value)
  (make-instance 'pdf-number :value value))

(def function satisfies-pdf-number-constraints? (value)
  (or (typep value '(signed-byte 32))
      (floatp value)))

(def pdf-ast-node string ()
  ((value :type string)))

(def function make-pdf-string (value)
  (make-instance 'pdf-string :value value))

(def pdf-ast-node name ()
  ((value :type string)))

(def function make-pdf-name (value)
  (make-instance 'pdf-name :value (if (symbolp value)
                                      (symbol-name value)
                                      value)))

(def pdf-ast-node array ()
  ((value :type list)))

(def pdf-ast-node stream ()
  ((contents :type list)))

(def pdf-ast-node begin-text ()
  ())

(def pdf-ast-node end-text ()
  ())

(def pdf-ast-node set-font ()
  ((name :type string)
   (size :type float)))

;; TODO: this is actually move text
(def pdf-ast-node move-text ()
  ((x :type float)
   (y :type float)))

(def pdf-ast-node display-text ()
  ())

(def pdf-ast-node dictionary ()
  ((map (make-hash-table) :type hash-table)))

(def pdf-ast-node catalog (pdf-dictionary)
  ())

(def pdf-ast-node pages (pdf-dictionary)
  ())

(def pdf-ast-node page (pdf-dictionary)
  ())

(def pdf-ast-node root (pdf-indirect-object)
  ((content)))

(def pdf-ast-node info (pdf-indirect-object)
  ((content)))

(def class* pdf-position-mixin ()
  ((position 0 :type integer)))

(def pdf-ast-node xref-entry (pdf-object-identifier pdf-position-mixin)
  ((free :type boolean)))

(def function make-pdf-xref-entry (&key (free #f) object-id generation-number)
  (make-instance 'pdf-xref-entry :free free :object-id object-id :generation-number generation-number))

(def pdf-ast-node xref-section ()
  ((entries nil :type list)))

(def function make-pdf-xref-section (&rest entries)
  (make-instance 'pdf-xref-section :entries entries))

(def pdf-ast-node xref (pdf-syntax-node pdf-position-mixin)
  ((sections nil :type list)))

(def function make-pdf-xref (&rest sections)
  (make-instance 'pdf-xref :sections sections))

(def pdf-ast-node header ()
  ((version "1.4" :type string)))

(def pdf-ast-node trailer ()
  ((dictionary (make-trailer-dictionary) :type pdf-dictionary)))

(def function make-pdf-trailer ()
  (make-instance 'pdf-trailer))

(def function make-trailer-dictionary ()
  (bind ((map (make-hash-table)))
    (setf (gethash (make-pdf-name "Root") map)
          (make-pdf-unquote '(root-reference-of *pdf-environment*)))
    (setf (gethash (make-pdf-name "Info") map)
          (make-pdf-unquote '(info-reference-of *pdf-environment*)))
    (setf (gethash (make-pdf-name "Size") map)
          (make-bivalent-unquote '(princ-to-string (compute-xref-size (xref-of *pdf-environment*)))))
    (make-instance 'pdf-dictionary :map map)))

(def pdf-ast-node document ()
  ((header (make-instance 'pdf-header) :type pdf-header)
   (elements :type list)
   (xref (make-pdf-xref
          (make-pdf-xref-section
           (make-pdf-xref-entry :free #t
                                :object-id 0
                                :generation-number 65535)))
         :type pdf-xref)
   (trailer (make-pdf-trailer) :type pdf-trailer)))

(def function make-pdf-document (elements)
  (make-instance 'pdf-document :elements elements))
