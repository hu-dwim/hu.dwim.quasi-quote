;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def special-variable *js-macros*               (make-hash-table :test 'eq))
(def special-variable *js-symbol-macros*        (make-hash-table :test 'eq))
(def special-variable *js-special-forms*        (make-hash-table :test 'eq))
(def special-variable *js-literals*             (make-hash-table :test 'eq))
(def special-variable *js-special-forms*        (make-hash-table :test 'eq))
;;(def special-variable *js-operator-name->arity* (make-hash-table :test 'eq))

(def (function io) js-special-form? (name)
  (nth-value 1 (gethash name *js-special-forms*)))

(def (function io) js-function-name? (name)
  (declare (ignore name))
  #t)

(def (function io) js-macro-name? (name &optional env)
  (declare (ignore env))
  (and (not (js-special-form? name))
       (nth-value 1 (gethash name *js-macros*))))

(def function js-symbol-macro-name? (name &optional env)
  (and (not (js-special-form? name))
       (nth-value 1 (macroexpand-1 name env))))

(def function js-macroexpand-1 (form &optional env)
  (declare (ignore env)) ; TODO check the env for macrolets?
  (bind ((name (first form))
         (args (rest form))
         (expander (gethash name *js-macros*)))
    (if expander
        (values (funcall expander args) #t)
        (values form #f))))

(def (definer e :available-flags "e") js-macro (name args &rest body)
  "Define a javascript macro, and store it in the toplevel macro environment."
  ;; TODO (undefine-js-compiler-macro name)
  (with-unique-names (arg-values)
    (with-standard-definer-options name
      `(progn
         (when (gethash ',name *js-macros*)
           (simple-style-warning "Redefining js macro ~S" ',name))
         (setf (gethash ',name *js-macros*)
               (lambda (,arg-values)
                 (destructuring-bind ,args ,arg-values ,@body)))
         ',name))))

(def definer js-literal (name string)
  `(progn
     (setf (gethash ',name *js-literals*) ,string)))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name js-name) :in entries)
                        (collect `(def js-literal ,name ,js-name))))))
  (frob
   (this      "this")
   (t         "true")
   (true      "true")
   (false     "false")
   (nil       "null")
   (undefined "undefined")))

(def definer js-special-form (name &body body)
  `(setf (gethash ',name *js-special-forms*)
         (lambda (-node-)
           (declare (ignorable -node-))
           (flet ((recurse (form)
                    (transform-quasi-quoted-js-to-quasi-quoted-string form)))
             (declare (ignorable #'recurse))
             ,@body))))

(def special-variable *js-operator-name->precedence*
  (bind ((result (make-hash-table :test 'eq)))
    (iter
      (for precedence :upfrom 1)
      (for operators :in '((js-aref)
                           (js-slot-value)
                           (! not ~)
                           (* / %)
                           (+ -)
                           (<< >>)
                           (>>>)
                           (< > <= >=)
                           (in js-expression-if)
                           (eql == != =)
                           (=== !==)
                           (&)
                           (^)
                           (\|)
                           (\&\& and)
                           (\|\| or)
                           (js-assign *= /= %= += -= <<= >>= >>>= \&= ^= \|=)
                           (comma)))
      (dolist (operator operators)
        (setf (gethash operator result) precedence)))
    result))

(def function operator-precedence (op)
  (gethash op *js-operator-name->precedence*))

(def function js-operator-name? (name)
  (not (null (operator-precedence name))))
