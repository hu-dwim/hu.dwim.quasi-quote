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
(def special-variable *js-unique-counter*       (or (when (boundp '*js-unique-counter*)
                                                      *js-unique-counter*)
                                                    ;; protect it from being reseted at an accidental reload
                                                    0))
;;(def special-variable *js-operator-name->arity* (make-hash-table :test 'eq))

(def (function ie) unique-js-name (&optional (prefix "g"))
  (concatenate 'string prefix (princ-to-string (incf *js-unique-counter*))))

(def (macro e) with-unique-js-names (names &body body)
  `(bind (,@(iter (for name :in names)
                  (collect `(,name (quote ,(make-symbol (unique-js-name (string name))))))))
     ,@body))

(def (function io) js-special-form? (name)
  (nth-value 1 (gethash name *js-special-forms*)))

(def (function io) js-literal-name? (name)
  (nth-value 1 (gethash name *js-literals*)))

(def (function io) js-function-name? (name)
  (declare (ignore name))
  #t)

(def (function io) js-macro-name? (name &optional env)
  (declare (ignore env))
  (and (not (js-special-form? name))
       (not (null (js-macro-definition name)))))

(def function js-symbol-macro-name? (name &optional env)
  (and (not (js-special-form? name))
       (nth-value 1 (macroexpand-1 name env))))

(def function js-macroexpand-1 (form &optional env)
  (declare (ignore env)) ; TODO check the env for macrolets?
  (bind ((name (first form))
         (args (rest form))
         (expander (js-macro-definition name)))
    (if expander
        (values (funcall expander args) #t)
        (values form #f))))

(def function js-macro-definition (name)
  (gethash name *js-macros*))

(def function (setf js-macro-definition) (value name)
  (when (gethash name *js-macros*)
    (simple-style-warning "Redefining js macro ~S" name))
  (setf (gethash name *js-macros*) value))

(def (definer e :available-flags "e") js-macro (name args &rest body)
  "Define a javascript macro, and store it in the toplevel macro environment."
  ;; TODO (undefine-js-compiler-macro name)
  (with-unique-names (arg-values)
    (with-standard-definer-options name
      `(progn
         (setf (js-macro-definition ',name)
               (lambda (,arg-values)
                 (destructuring-bind ,args ,arg-values ,@body)))
         ',name))))

(def (definer e :available-flags "e") js-lisp-macro-alias (lisp-name &optional (js-name (intern (string-downcase lisp-name))))
  (with-standard-definer-options js-name
    `(setf (js-macro-definition ',js-name)
           (lambda (args)
             (macroexpand `(,',lisp-name ,@args))))))

(def (definer :available-flags "e") js-literal (name string)
  (bind ((lowercase-name (intern (string-downcase name))))
    `(progn
       (setf (gethash ',name *js-literals*) ,string)
       (setf (gethash ',lowercase-name *js-literals*) ,string)
       ,@(when (getf -options- :export)
           `((export '(,name ,lowercase-name)))))))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name js-name) :in entries)
                        (collect `(def (js-literal e) ,name ,js-name))))))
  (frob
   (this      "this")
   (t         "true")
   (true      "true")
   (false     "false")
   (nil       "null")
   (undefined "undefined")))

(def macro with-lexical-transform-functions (&body body)
  `(labels ((recurse (form)
              (bind ((*in-js-statement-context* #f))
                (transform-quasi-quoted-js-to-quasi-quoted-string form)))
            (recurse-as-comma-separated (form &optional (recurse-fn #'recurse))
              (bind ((recurse-fn (ensure-function recurse-fn)))
                (iter (for el :in form)
                      (unless (first-iteration-p)
                        (collect ", "))
                      (collect (funcall recurse-fn el))))))
     (declare (ignorable #'recurse #'recurse-as-comma-separated))
     ,@body))

(def (definer :available-flags "e") js-special-form (name &body body)
  (with-standard-definer-options name
    `(setf (gethash ',name *js-special-forms*)
           (named-lambda ,(symbolicate '#:js-special-form/ name) (-node-)
             (declare (ignorable -node-))
             (with-lexical-transform-functions
               ,@body)))))

(def special-variable *js-operator-name->precedence*
  (bind ((result (make-hash-table :test 'eq)))
    (iter
      (for precedence :upfrom 1)
      ;; TODO whats up with: js-expression-if, js-assign, comma, js-aref, js-slot-value?
      (for operators :in '(;;(js-aref)
                           ;;(js-slot-value)
                           (! |not| ~)
                           (* / %)
                           (+ -)
                           (<< >>)
                           (>>>)
                           (< > <= >=)
                           (|in| |if|)
                           (|eql| == != =)
                           (|eq| === !==)
                           (&)
                           (^)
                           (\|)
                           (\&\& |and|)
                           (\|\| |or|)
                           (|js-assign| *= /= %= += -= <<= >>= >>>= \&= ^= \|=)
                           (|comma|)))
      (dolist (operator operators)
        (export operator :cl-quasi-quote-js)
        (setf (gethash operator result) precedence)))
    result))

(def function operator-precedence (op)
  (gethash op *js-operator-name->precedence*))

(def function js-operator-name? (name)
  (not (null (operator-precedence name))))
