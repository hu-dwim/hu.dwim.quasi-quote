;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-ui)

;;;;;;;
;;; AST

(def ast ui)

(def class* ui-syntax-node (syntax-node parent-mixin)
  ())

(def (class* e) ui-quasi-quote (quasi-quote ui-syntax-node)
  ())

(def (function e) make-ui-quasi-quote (body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'ui-quasi-quote :body body))

(def (class* e) ui-unquote (unquote ui-syntax-node)
  ())

(def (function e) make-ui-unquote (form &optional (spliced? #f))
  (make-instance 'ui-unquote :form form :spliced spliced?))

(def special-variable *ui-ast-node-name->sexp-parser* (make-hash-table :test #'eq))

(def definer ui-ast-node-parser (name &body body)
  `(setf (gethash ',name *ui-ast-node-name->sexp-parser*)
         (lambda (-sexp-)
           (declare (ignorable -sexp-))
           (block nil
             ,@body))))

(def definer ui-ast-node (name supers slots &rest options)
  (bind ((class-name (format-symbol #.(find-package :cl-quasi-quote-ui) "UI-~A" name))
         (defclass-form `(def class* ,class-name ,(or supers '(ui-syntax-node)) ,slots ,@options))
         (expanded-slots (fourth (macroexpand-1 defclass-form)))
         (slot-accessors (mapcar (lambda (slot-definition)
                                   (getf (rest slot-definition) :accessor))
                                 expanded-slots)))
    `(progn
       ,defclass-form
       (def ui-ast-node-parser ,name
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

;;;;;;;
;;; AST

(def ui-ast-node screen (ui-syntax-node)
  ((content)))

(def ui-ast-node list (ui-syntax-node)
  ((orientation :vertical :type (member :horizontal :vertical))
   (elements)))

(def ui-ast-node vertical-list (ui-list)
  ()
  (:default-initargs :orientation :vertical))

(def ui-ast-node horizontal-list (ui-list)
  ()
  (:default-initargs :orientation :horizontal))

(def ui-ast-node text (ui-syntax-node)
  ((contents)))

(def ui-ast-node menu (ui-syntax-node)
  ((menu-items)))

(def ui-ast-node content-menu (ui-menu ui-list)
  ((place)))

(def ui-ast-node menu-item (ui-syntax-node)
  ((label)
   (action)))

(def ui-ast-node action (ui-syntax-node)
  ((label)
   (action)))

(def ui-ast-node text-field (ui-syntax-node)
  ((place)))

(def ui-ast-node form (ui-syntax-node)
  ((content)))

(def ui-ast-node table (ui-syntax-node)
  ((rows)))

(def ui-ast-node row (ui-syntax-node)
  ((cells)))

(def ui-ast-node cell (ui-syntax-node)
  ((content)))
