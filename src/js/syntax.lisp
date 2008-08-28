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
                                     (transformation-pipeline nil)
                                     (nested-transformation-pipeline nil)
                                     (dispatched-quasi-quote-name "js"))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (bind ((toplevel? (= 1 *quasi-quote-nesting-level*)))
       `(,(if toplevel? 'js-quasi-quote/toplevel 'js-quasi-quote)
          ,(= 1 *quasi-quote-depth*)
          ,body
          ,(if (= 1 *quasi-quote-depth*)
               transformation-pipeline
               (or nested-transformation-pipeline
                   transformation-pipeline)))))
   (lambda (body spliced?)
     `(js-unquote ,body ,spliced?))
   :start-character start-character
   :dispatch-character dispatch-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :readtable-case :preserve
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
                                               (dispatched-quasi-quote-name "js")
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-js-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                           :start-character start-character
                                                           :end-character end-character
                                                           :unquote-character unquote-character
                                                           :splice-character splice-character
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

(def (function e) make-quasi-quoted-js-to-form-emitting-transformation-pipeline
    (stream-variable-name &key binary with-inline-emitting indentation-width (encoding :utf-8) output-prefix output-postfix declarations)
  (if binary
      (list (make-instance 'quasi-quoted-js-to-quasi-quoted-string
                           :output-prefix output-prefix
                           :output-postfix output-postfix
                           :indentation-width indentation-width)
            (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                           :encoding encoding)
            (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))
      (list (make-instance 'quasi-quoted-js-to-quasi-quoted-string
                           :output-prefix output-prefix
                           :output-postfix output-postfix
                           :indentation-width indentation-width)
            (make-instance 'quasi-quoted-string-to-string-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))))

(def reader-stub js-quasi-quote (toplevel? form transformation-pipeline)
  (bind ((expanded-body (recursively-macroexpand-reader-stubs form -environment-))
         (quasi-quote-node (make-js-quasi-quote transformation-pipeline (walk-js expanded-body))))
    (if toplevel?
        (run-transformation-pipeline quasi-quote-node)
        quasi-quote-node)))

(def reader-stub js-unquote (body spliced?)
  ;; A macro to handle the quoted parts when the walker is walking the forms. Kinda like a kludge, but it's not that bad...
  (make-js-unquote body spliced?))

(def method find-walker-handler ((ast-node syntax-node))
  (constantly ast-node))

(def special-variable *js-walker-handlers* (make-hash-table :test #'eq))

(defun find-js-walker-handler (name)
  (or (and (consp name)
           (gethash (first name) *js-walker-handlers*))
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

(defun undefined-js-reference-handler (type name)
  (declare (ignore type name))
  #+nil ; they are simply too common in js, so just ignore them
  (unless (member name '("document" "debugger") :test #'string=)
    (cl-walker::undefined-reference-handler type name)))

(def function walk-js (form &optional lexenv)
  (with-walker-configuration (:undefined-reference-handler 'undefined-js-reference-handler
                              :function-name?      'js-function-name?
                              :macro-name?         'js-macro-name?
                              :symbol-macro-name?  'js-symbol-macro-name?
                              :constant-name?      'js-constant-name?
                              :lambda-form?        'js-lambda-form?
                              :macroexpand-1       'js-macroexpand-1
                              :find-walker-handler 'find-js-walker-handler)
    (walk-form form nil (make-walk-environment lexenv))))


;;;;;;;;;;;;;;
;;; conditions

(def (condition* e) js-compile-error (error)
  ((walked-form nil)))

(def condition* simple-js-compile-error (js-compile-error simple-error)
  ())

(def function simple-js-compile-error (walked-form message &rest args)
  (error 'simple-js-compile-error :walked-form walked-form :format-control message :format-arguments args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some js specific handlers

(def class* function-definition-form (lambda-function-form)
  ((name)))

(def js-walker-handler |defun| (form parent env)
  (bind (((name args &rest body) (rest form)))
    (with-form-object (result function-definition-form
                              :parent parent
                              :source form)
      (walk-lambda-like result
                        (nsubstitute '&optional '|&optional|
                                     (nsubstitute '&allow-other-keys '|&allow-other-keys|
                                                  (substitute '&key '|&key| args)))
                        body env)
      (setf (name-of result) name))))

;; cl:lambda is a macro that expands to (function (lambda ...)), so we need to define our own handler here
(def (js-walker-handler e) |lambda| (form parent env)
  (walk-lambda form parent env))

(def (js-walker-handler e) |return| (form parent env)
  (unless (<= 1 (length form) 2)
    (simple-walker-error "Illegal return form: ~S" form))
  (let ((value (second form)))
    (with-form-object (return-from-node return-from-form :parent parent :source form)
      (setf (result-of return-from-node) (when value
                                           (walk-form value return-from-node env))))))

(def class* create-form (walked-form)
  ((elements)))

(def js-walker-handler |create| (form parent env)
  (unless (evenp (length (rest form)))
    (simple-js-compile-error nil "Odd elements in create form ~S" form))
  (let ((elements (rest form)))
    (with-form-object (create-node create-form :parent parent :source form)
      (setf (elements-of create-node)
            (iter (for (name value) :on elements :by #'cddr)
                  (collect (cons name (walk-form value create-node env))))))))

(def class* slot-value-form (walked-form)
  ((object)
   (slot-name)))

(def (js-walker-handler e) |slot-value| (form parent env)
  (unless (length= 2 (rest form))
    (simple-js-compile-error nil "Invalid slot-value form" form))
  (with-form-object (node slot-value-form :parent parent :source form)
    (setf (object-of node) (walk-form (second form) node env))
    (setf (slot-name-of node) (bind ((slot-name (third form)))
                                (if (quoted-symbol? slot-name)
                                    (second slot-name)
                                    (walk-form slot-name node env))))))

(def class* instantiate-form (walked-form)
  ((type-to-instantiate)
   (arguments)))

(def (js-walker-handler e) |new| (form parent env)
  (when (< (length (rest form)) 2)
    (simple-js-compile-error nil "Invalid 'new' form, needs at least two elements: ~S" form))
  (bind ((type (second form))
         (args (cddr form)))
    (with-form-object (node instantiate-form :parent parent :source form)
      (setf (type-to-instantiate-of node) type)
      (setf (arguments-of node) (mapcar [walk-form !1 node env] args)))))

(def class* try-form (walked-form implicit-progn-mixin)
  ((catch-clauses)
   (finally-clause)))

(def (js-walker-handler e) |try| (form parent env)
  (when (< (length (rest form)) 2)
    (simple-js-compile-error nil "Invalid 'try' form, needs at least two elements: ~S" form))
  (with-form-object (node try-form :parent parent :source form)
    (bind ((body (second form))
           (catch-clauses (copy-list (rest (rest form))))
           (finally-clause (bind ((finally (assoc '|finally| catch-clauses)))
                             (when finally
                               (setf catch-clauses (remove-if [eq (first !1) '|finally|] catch-clauses))
                               (rest finally)))))
      (setf (finally-clause-of node) (mapcar [walk-form !1 node env] finally-clause))
      (setf (catch-clauses-of node)  (mapcar [walk-form !1 node env] catch-clauses))
      (setf (cl-walker:body-of node) (walk-form body node env)))))

(def class* catch-form (walked-form implicit-progn-mixin)
  ((variable-name)
   (condition)))

(def (js-walker-handler e) |catch| (form parent env)
  (when (< (length (rest form)) 2)
    (simple-js-compile-error nil "Invalid 'catch' form, needs at least two elements: ~S" form))
  (bind (((nil (variable-name &rest condition) &body body) form))
    (unless (and variable-name
                 (symbolp variable-name))
      (simple-js-compile-error nil "The condition variable in a 'catch' form must be a symbol. Got ~S instead." variable-name))
    (with-form-object (node catch-form :parent parent :source form)
      (setf (variable-name-of node) variable-name)
      (setf (condition-of node) (when condition
                                  (walk-form condition node env)))
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

;; reinstall some cl handlers on the same, but lowercase symbol exported from cl-quasi-quote-js
;; because `js is case sensitive...
(progn
  (dolist (symbol {(with-readtable-case :preserve)
                   ;; NOTE lambda needs its own handler, see above
                   '(progn let let* setf setq defun block return if unwind-protect)})
    (export symbol :cl-quasi-quote-js)
    (bind ((cl-symbol (find-symbol (string-upcase (symbol-name symbol)) :common-lisp)))
      (assert cl-symbol)
      (awhen (gethash cl-symbol cl-walker::*walker-handlers*)
        (setf (gethash symbol *js-walker-handlers*) it))))

  (setf (gethash '|setf| *js-walker-handlers*) (gethash 'setq cl-walker::*walker-handlers*)))
