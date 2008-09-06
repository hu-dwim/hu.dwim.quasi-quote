;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def special-variable *reader-stub-expanders* (make-hash-table))

(def definer reader-stub (name args &body body)
  (with-unique-names (arg-values)
    `(progn
       (setf (find-reader-stub ',name)
             (named-lambda ,(format-symbol *package* "READER-STUB/~A" name) (,arg-values -environment-)
               (declare (ignorable -environment-))
               (destructuring-bind ,args ,arg-values
                 ,@body)))
       ;; define a macro that is actually a macro, so the reader can read into a macro form with this
       ;; which will start the whole transformation. this way when walking the unquoted forms, libs like iter will
       ;; not think that nested quasi-quote forms are macros...
       (def macro ,(format-symbol *package* "~A/TOPLEVEL" name) (&whole whole &rest args &environment environment)
         (declare (ignore args))
         (expand-reader-stub (cons ',name (rest whole)) environment)))))

(def function find-reader-stub (name &key (otherwise :error))
  (or (gethash name *reader-stub-expanders*)
      (handle-otherwise otherwise)))

(def function (setf find-reader-stub) (value name)
  (setf (gethash name *reader-stub-expanders*) value))

(def function expand-reader-stub (form environment)
  (macroexpand (funcall (find-reader-stub (first form)) (rest form) environment) environment))

(def function recursively-macroexpand-reader-stubs (form &optional env)
  (typecase form
    (cons
     (if (find-reader-stub (first form) :otherwise nil)
         (recursively-macroexpand-reader-stubs (expand-reader-stub form env) env)
         (iter (for entry :first form :then (cdr entry))
               (collect (recursively-macroexpand-reader-stubs (car entry) env) :into result)
               (cond
                 ((consp (cdr entry))
                  ;; nop, go on looping
                  )
                 ((cdr entry)
                  (setf (cdr (last result)) (recursively-macroexpand-reader-stubs (cdr entry) env))
                  (return result))
                 (t (return result))))))
    ;; also process the forms of unquote ast nodes
    (unquote (bind ((unquote-node form))
               (setf (form-of unquote-node)
                     (recursively-macroexpand-reader-stubs
                      ;; first descend into the unquoted forms and macroexpand all the macros
                      ;; except the quasi-quote and unquote reader wrappers which are reader-stub's
                      ;; so they are not expanded by the walker. this is needed
                      ;; to flatten the lisp `(foo ,bar) used inside lisp macros (macrolet's),
                      ;; so that later the RECURSIVELY-MACROEXPAND-READER-STUBS call can properly
                      ;; walk the simple expanded lists.
                      (cl-walker:with-walker-configuration (:undefined-reference-handler nil)
                        (cl-walker:macroexpand-all (form-of unquote-node) env))
                      env))
               unquote-node))
    (t form)))

;;;;;;;
;;; AST

(def (class* e) syntax-node ()
  ())

;; TODO ? maybe it's just a thinko that it's needed
(def method cl-walker:unwalk-form ((self syntax-node))
  self)

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
  (iter (with class = (class-of -self-))
        (for slot :in (class-slots class))
        (when (slot-boundp-using-class class -self- slot)
          (bind ((value (slot-value-using-class class -self- slot)))
            ;; TODO: this is really fragile
            (typecase value
              (parent-mixin (setf (parent-of value) -self-))
              (list
               (when (eq 'list (slot-definition-type slot))
                 (dolist (element value)
                   (setf (parent-of element) -self-))))
              (hash-table
               (iter (for (key value) :in-hashtable value)
                     (when (typep value 'parent-mixin)
                       (setf (parent-of value) -self-))
                     (when (typep key 'parent-mixin)
                       (setf (parent-of key) -self-)))))))))

(def function find-ancestor-syntax-node (node type)
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
  (bind ((class (class-of -self-))
         (*ast-print-object-nesting-level* (1+ *ast-print-object-nesting-level*)))
    (if (> *ast-print-object-nesting-level* +ast-print-depth+)
        (write-string "...")
        (iter (for slot :in (class-slots class))
              (when (slot-boundp-using-class class -self- slot)
                (for value = (slot-value-using-class class -self- slot))
                (unless (first-iteration-p)
                  (write-string " "))
                (write (first (slot-definition-initargs slot)))
                (write-string " ")
                (write value))))))
