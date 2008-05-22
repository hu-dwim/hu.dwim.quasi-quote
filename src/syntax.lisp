;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def (macro e) with-local-readtable (&body body)
  "Rebind a copy of *readtable*, mostly for REPL use."
  `(bind ((*readtable* (copy-readtable *readtable*)))
     ,@body))

(def function recursively-macroexpand-reader-stubs (form &optional env)
  (typecase form
    (cons
     (bind ((candidate (first form))
            (ast-node-class (when (and candidate
                                       (symbolp candidate))
                              (find-class candidate nil))))
       (unless (subtypep ast-node-class 'quasi-quote)
         (setf ast-node-class nil))
       (if ast-node-class
           (macroexpand form env)
           (iter (for entry :first form :then (cdr entry))
                 (collect (recursively-macroexpand-reader-stubs (car entry)) :into result)
                 (cond
                   ((consp (cdr entry))
                    ;; nop, go on looping
                    )
                   ((cdr entry)
                    (setf (cdr (last result)) (recursively-macroexpand-reader-stubs (cdr entry)))
                    (return result))
                   (t (return result)))))))
    (unquote (bind ((unquote-node form))
               (setf (form-of unquote-node) (recursively-macroexpand-reader-stubs (form-of unquote-node)))
               unquote-node))
    (t form)))

#+nil                                   ; TODO delme eventually
(def (function e) with-transformed-quasi-quoted-syntax (&rest transformatations)
  (lambda (reader)
    (bind (((name &rest args) (ensure-list (first transformatations))))
      (chain-transform (cdr transformatations)
                       (funcall (apply (format-symbol (symbol-package name)  "WITH-~A-SYNTAX" name) args)
                                reader)))))

;;;;;;;
;;; AST

(def (class* e) syntax-node ()
  ())

(def (class* e) quasi-quote (syntax-node)
  ((transformation-pipeline)
   (body)))

(def (class* e) unquote (syntax-node)
  ((form)
   (spliced #f :type boolean)))

;; TODO: eliminate side effect and check for returning (values) from unqutes
;; TODO: what if the unquote returns with another function
(def (class* e) side-effect (syntax-node)
  ((form)))

(def (function e) make-side-effect (form)
  (make-instance 'side-effect :form form))

;; TODO: revise this, how to make it safe
(def (class* e) parent-mixin ()
  ((parent :type syntax-node)))

(def constructor parent-mixin
  (iter (with class = (class-of self))
        (for slot :in (class-slots class))
        (when (slot-boundp-using-class class self slot)
          (bind ((value (slot-value-using-class class self slot)))
            ;; TODO: this is really fragile
            (typecase value
              (parent-mixin (setf (parent-of value) self))
              (list
               (when (eq 'list (slot-definition-type slot))
                 (dolist (element value)
                   (setf (parent-of element) self))))
              (hash-table
               (iter (for (key value) :in-hashtable value)
                     (when (typep value 'parent-mixin)
                       (setf (parent-of value) self))
                     (when (typep key 'parent-mixin)
                       (setf (parent-of key) self)))))))))

(def function find-ancestor (node type)
  (iter (for current :initially node :then (parent-of current))
        (until (typep current type))
        (finally (return current))))

(def function ast-package (name)
  (bind ((package (symbol-package name)))
    (if (eq package (find-package :common-lisp))
        (find-package :cl-quasi-quote)
        package)))

(def (definer e) ast (name)
  (bind ((package (ast-package name)))
    (flet ((process (names)
             (apply 'format-symbol package
                    (iter (repeat (length names))
                          (collect #\~ :result-type string)
                          (collect #\A :result-type string))
                    names)))
      `(export
        ',(mapcar #'process
                  `((,name)
                    ("QUASI-QUOTED-" ,name)
                    (,name "-EMITTING-FORM")))
        ,package))))

(export '(lambda-form lambda))

(def method make-load-form ((instance syntax-node) &optional environment)
  (make-load-form-saving-slots instance :environment environment))

(def special-variable *ast-print-object-nesting-level* 0)

(def constant +ast-print-depth+ 2)

(def print-object syntax-node
  (bind ((class (class-of self))
         (*ast-print-object-nesting-level* (1+ *ast-print-object-nesting-level*)))
    (if (> *ast-print-object-nesting-level* +ast-print-depth+)
        (write-string "...")
        (iter (for slot :in (class-slots class))
              (when (slot-boundp-using-class class self slot)
                (for value = (slot-value-using-class class self slot))
                (unless (first-iteration-p)
                  (write-string " "))
                (write (first (slot-definition-initargs slot)))
                (write-string " ")
                (write value))))))
