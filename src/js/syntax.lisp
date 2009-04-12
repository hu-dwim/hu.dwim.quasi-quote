;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(define-syntax quasi-quoted-js (&key start-character
                                     end-character
                                     dispatch-character
                                     (unquote-character #\,)
                                     (splice-character #\@)
                                     (destructive-splice-character #\.)
                                     (transformation-pipeline nil)
                                     (dispatched-quasi-quote-name "js")
                                     (toplevel-reader-wrapper #'identity))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (bind ((toplevel? (= 1 *quasi-quote-nesting-level*))
            (quasi-quote-node (make-js-quasi-quote (coerce-to-transformation-pipeline transformation-pipeline)
                                                   (walk-js body))))
       (if toplevel?
           `(toplevel-quasi-quote-macro ,quasi-quote-node)
           quasi-quote-node)))
   (lambda (body modifier)
     (make-js-unquote body modifier))
   :start-character start-character
   :dispatch-character dispatch-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character
   :readtable-case :preserve
   :unquote-readtable-case :toplevel
   :toplevel-reader-wrapper toplevel-reader-wrapper
   :dispatched-quasi-quote-name dispatched-quasi-quote-name))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-JS-TO-~A" name))
                    (&key-position (position '&key args)))
               `(define-syntax ,syntax-name (,@(subseq args 0 (or &key-position (length args)))
                                               &key
                                               (with-inline-emitting #f)
                                               (declarations '())
                                               (output-prefix nil)
                                               (output-postfix nil)
                                               (indentation-width nil)
                                               (start-character #\<)
                                               (end-character #\>)
                                               (unquote-character #\,)
                                               (splice-character #\@)
                                               (destructive-splice-character #\.)
                                               (dispatched-quasi-quote-name "js")
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-js-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                           :start-character start-character
                                                           :end-character end-character
                                                           :unquote-character unquote-character
                                                           :splice-character splice-character
                                                           :destructive-splice-character destructive-splice-character
                                                           :dispatched-quasi-quote-name dispatched-quasi-quote-name)))))
  ;; TODO ? (x js-emitting-form            '(js-emitting-form))
  (x string-emitting-form (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary #f
                           :with-inline-emitting with-inline-emitting
                           :indentation-width indentation-width
                           :output-prefix output-prefix
                           :output-postfix output-postfix
                           :declarations declarations)
     (stream-variable-name))
  (x binary-emitting-form (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary #t
                           :with-inline-emitting with-inline-emitting
                           :indentation-width indentation-width
                           :encoding encoding
                           :output-prefix output-prefix
                           :output-postfix output-postfix
                           :declarations declarations)
     (stream-variable-name &key
                           (encoding *default-character-encoding*))))

(def method find-walker-handler ((ast-node syntax-node))
  (constantly ast-node))

(def special-variable *js-walker-handlers* (make-hash-table :test #'eq))

(defun find-js-walker-handler (name)
  (or (and (consp name)
           (bind (((:values handler found?) (gethash (first name) *js-walker-handlers*)))
             (when found?
               (assert (not (null handler)))
               handler)))
      (find-walker-handler name)))

(def (definer :available-flags "e") js-walker-handler (name (form parent lexenv) &body body)
  (with-standard-definer-options name
    `(bind ((cl-walker::*walker-handlers* *js-walker-handlers*))
       (defwalker-handler ,name (,form ,parent ,lexenv)
         ,@body))))

(def function js-constant-name? (form &optional env)
  (declare (ignore env))
  (or (gethash form *js-literals*)
      (and (not (symbolp form))
           (not (consp form)))))

(def function js-lambda-form? (form &optional env)
  (declare (ignore env))
  (and (consp form)
       (member (car form) '(cl:lambda |lambda|))
       #t))

(def function js-lambda-like-walker (ast-node args body env)
  (cl-walker::%walk-lambda-like ast-node (%fixup-lambda-list args) body env))

(defun undefined-js-reference-handler (type name)
  (declare (ignore type name))
  #+nil ; they are simply too common in js, so just ignore them
  (unless (member name '("document" "debugger") :test #'string=)
    (cl-walker::undefined-reference-handler type name)))

(def function walk-js (form &optional lexenv)
  (labels ((recurse (x)
             (typecase x
               (list-quasi-quote
                (run-transformation-pipeline x))
               (cons (cons (recurse (car x))
                           (recurse (cdr x))))
               (t x))))
    ;; let's transform all list qq nodes inside the form (this handles ` in macrolets in js forms)
    (setf form (recurse form)))
  (with-walker-configuration (:undefined-reference-handler 'undefined-js-reference-handler
                              :function-name?      'js-function-name?
                              :macro-name?         'js-macro-name?
                              :symbol-macro-name?  'js-symbol-macro-name?
                              :constant-name?      'js-constant-name?
                              :lambda-form?        'js-lambda-form?
                              :lambda-like-walker  'js-lambda-like-walker
                              :macroexpand-1       'js-macroexpand-1
                              :find-walker-handler 'find-js-walker-handler)
    (walk-form form nil (make-walk-environment lexenv))))


;;;;;;;;;;;;;;
;;; conditions

(def (condition* e) js-compile-condition ()
  ((walked-form nil)))

(def (condition* e) js-compile-error (js-compile-condition error)
  ())

(def condition* simple-js-compile-error (js-compile-error simple-error)
  ())

(def function js-compile-error (walked-form message &rest args)
  (declare (type string message)
           (type (or null syntax-node walked-form) walked-form))
  (error 'simple-js-compile-error :walked-form walked-form :format-control message :format-arguments args))


(def (condition* e) js-compile-warning (js-compile-condition warning)
  ())

(def condition* simple-js-compile-warning (js-compile-warning simple-warning)
  ())

(def function js-compile-warning (walked-form message &rest args)
  (declare (type string message)
           (type (or null syntax-node walked-form) walked-form))
  (error 'simple-js-compile-warning :walked-form walked-form :format-control message :format-arguments args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some js specific handlers

(def class* function-definition-form (lambda-function-form)
  ((name)))

(def function %fixup-lambda-list (args)
  ;; this is kinda hackish, but does what we want
  (nsubstitute '&rest '|&rest|
               (nsubstitute '&optional '|&optional|
                            (nsubstitute '&allow-other-keys '|&allow-other-keys|
                                         (substitute '&key '|&key| args)))))

(def js-walker-handler |defun| (form parent env)
  (bind (((name args &rest body) (rest form)))
    (with-form-object (result 'function-definition-form parent
                              :source form)
      (walk-lambda-like result (%fixup-lambda-list args) body env)
      (setf (name-of result) name))))

;; cl:lambda is a macro that expands to (function (lambda ...)), so we need to define our own handler here
(def (js-walker-handler e) |lambda| (form parent env)
  (walk-form `(function ,form) parent env))

(def (js-walker-handler e) |return| (form parent env)
  (unless (<= 1 (length form) 2)
    (simple-walker-error "Illegal return form: ~S" form))
  (let ((value (second form)))
    (with-form-object (return-from-node 'return-from-form parent
                                        :source form)
      (setf (result-of return-from-node) (when value
                                           (walk-form value return-from-node env))))))

(def class* for-form (walked-form)
  ((variables)
   (steps)
   (looping-condition)
   (body)))

(def js-walker-handler |do| (form parent env)
  (with-form-object (for-node 'for-form parent
                              :source form)
    (bind (((raw-variables (raw-end-test &optional result) &rest raw-body) (rest form)))
      (when result
        (js-compile-error for-node "DO can't handle a result expression"))
      (setf (values (variables-of for-node) (steps-of for-node))
            (iter (for entry :in raw-variables)
                  (for (var init step) = (ensure-list entry))
                  (collect (make-instance 'setq-form
                                          :parent for-node
                                          :source form
                                          :variable (walk-form var for-node env)
                                          :value (walk-form init for-node env))
                    :into variables)
                  (when step
                    (collect (walk-form `(setq ,var ,step) for-node env) :into steps))
                  (finally (return (values variables steps)))))
      (setf (looping-condition-of for-node) (walk-form `(|not| ,raw-end-test) for-node env))
      (setf (body-of for-node) (mapcar [walk-form !1 for-node env] raw-body)))))

(def class* create-form (walked-form)
  ((elements)))

(def (js-walker-handler e) |create| (form parent env)
  (bind ((elements (rest form)))
    (with-form-object (create-node 'create-form parent
                                   :source form)
      (setf (elements-of create-node) (mapcar [walk-form !1 create-node env] elements)))))

(def class* array-form (walked-form)
  ((elements)))

(def (js-walker-handler e) |array| (form parent env)
  (bind ((elements (rest form)))
    (with-form-object (toplevel-create-node 'array-form parent
                                            :source form)
      (labels ((recurse (node)
                 (if (and (vectorp node)
                          (not (stringp node)))
                     (with-form-object (create-node 'array-form parent
                                                    :source form)
                       (setf (elements-of toplevel-create-node) (map 'list #'recurse node)))
                     (walk-form node toplevel-create-node env))))
        (setf (elements-of toplevel-create-node) (mapcar #'recurse elements))))))

(def class* slot-value-form (walked-form)
  ((object)
   (slot-name)))

(def (js-walker-handler e) |slot-value| (form parent env)
  (unless (length= 2 (rest form))
    (js-compile-error nil "Invalid slot-value form" form))
  (with-form-object (node 'slot-value-form parent
                          :source form)
    (setf (object-of node) (walk-form (second form) node env))
    (setf (slot-name-of node) (bind ((slot-name (third form)))
                                (if (quoted-symbol? slot-name)
                                    (second slot-name)
                                    (walk-form slot-name node env))))))

(def class* instantiate-form (walked-form)
  ((type-to-instantiate)
   (arguments)))

(def (js-walker-handler e) |new| (form parent env)
  (when (< (length form) 2)
    (js-compile-error nil "Invalid 'new' form, needs at least two elements: ~S" form))
  (bind ((type (second form))
         (args (cddr form)))
    (with-form-object (node 'instantiate-form parent
                            :source form)
      (setf (type-to-instantiate-of node) type)
      (setf (arguments-of node) (mapcar [walk-form !1 node env] args)))))

(def class* try-form (walked-form)
  ((protected-form)
   (catch-clauses)
   (finally-clause)))

(def (js-walker-handler e) |try| (form parent env)
  (when (< (length (rest form)) 2)
    (js-compile-error nil "Invalid 'try' form, needs at least two elements: ~S" form))
  (with-form-object (node 'try-form parent
                          :source form)
    (bind ((body (second form))
           (catch-clauses (copy-list (rest (rest form))))
           (finally-clause (bind ((finally (assoc '|finally| catch-clauses)))
                             (when finally
                               (setf catch-clauses (remove-if [eq (first !1) '|finally|] catch-clauses))
                               (rest finally)))))
      (setf (finally-clause-of node) (mapcar [walk-form !1 node env] finally-clause))
      (setf (catch-clauses-of node)  (mapcar [walk-form !1 node env] catch-clauses))
      (setf (protected-form-of node) (walk-form body node env)))))

(def class* catch-form (walked-form implicit-progn-mixin)
  ((variable-name)
   (condition)))

(def (js-walker-handler e) |catch| (form parent env)
  (when (< (length (rest form)) 2)
    (js-compile-error nil "Invalid 'catch' form, needs at least two elements: ~S" form))
  (bind (((nil (variable-name &rest condition) &body body) form))
    (unless (and variable-name
                 (symbolp variable-name))
      (js-compile-error nil "The condition variable in a 'catch' form must be a symbol. Got ~S instead." variable-name))
    (with-form-object (node 'catch-form parent
                            :source form)
      (setf (variable-name-of node) variable-name)
      (setf (condition-of node) (when condition
                                  (walk-form condition node env)))
      (setf (cl-walker:body-of node) (mapcar [walk-form !1 node env] body)))))

(def class* while-form (walked-form implicit-progn-mixin)
  ((condition)))

(def (js-walker-handler e) |while| (form parent env)
  (when (< (length (rest form)) 2)
    (js-compile-error nil "Invalid 'while' form, needs at least two elements: ~S" form))
  (bind (((nil condition &body body) form))
    (with-form-object (node 'while-form parent
                            :source form)
      (setf (condition-of node) (walk-form condition node env))
      (setf (cl-walker:body-of node) (mapcar [walk-form !1 node env] body)))))

(def (js-walker-handler e) |macrolet| (form parent env)
  ;; this is a KLUDGE: the walker only understands &BODY but the js reader is case sensitive
  (funcall (find-walker-handler `(macrolet))
           `(macrolet (,@(iter (for (name args . body) :in (second form))
                               (collect `(,name ,(substitute '&key '|&key|
                                                             (substitute '&body '|&body| args))
                                                ,@body))))
              ,@(rest (rest form)))
           parent env))

(def class* type-of-form (walked-form)
  ((object)))

(def (js-walker-handler e) |type-of| (form parent env)
  (unless (length= 2 form)
    (js-compile-error nil "Invalid 'type-of' form, needs exactly one argument: ~S" form))
  (bind (((nil object) form))
    (with-form-object (node 'type-of-form parent :source form)
      (setf (object-of node) (walk-form object node env)))))

(def class* regexp-form (walked-form)
  ((regexp)))

(def (js-walker-handler e) |regexp| (form parent env)
  (bind ((regexp (second form)))
    (unless (and (length= 2 form)
                 (stringp regexp))
      (js-compile-error nil "Invalid 'regexp' form, needs exactly one argument, a string: ~S" form))
    (with-form-object (node 'regexp-form parent :source form)
      (setf (regexp-of node) regexp))))


;; reinstall some cl handlers on the same, but lowercase symbol exported from cl-quasi-quote-js
;; because `js is case sensitive...
(progn
  (dolist (symbol {with-preserved-readtable-case
                   ;; NOTE lambda needs its own handler, see above
                   '(progn let let* setf setq defun block return if unwind-protect flet)})
    (export symbol :cl-quasi-quote-js)
    (bind ((cl-symbol (find-symbol (string-upcase (symbol-name symbol)) :common-lisp)))
      (assert cl-symbol)
      (awhen (gethash cl-symbol cl-walker::*walker-handlers*)
        (setf (gethash symbol *js-walker-handlers*) it))))

  (macrolet ((js-to-lisp-handler-alias (new existing)
               `(setf (gethash ',new *js-walker-handlers*) (gethash ',existing cl-walker::*walker-handlers*))))
    (js-to-lisp-handler-alias |setf| setq))

  (macrolet ((js-handler-alias (new existing)
               `(progn
                  (setf (gethash ',new *js-walker-handlers*) (gethash ',existing *js-walker-handlers*))
                  (export ',new))))
    (js-handler-alias |typeof| |type-of|)
    (js-handler-alias type-of  |type-of|)))
