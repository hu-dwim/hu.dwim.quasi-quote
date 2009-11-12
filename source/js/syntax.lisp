;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.js)

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

(def special-variable *js-walker-handlers* (make-hash-table :test #'eq))

(def (definer :available-flags "e") js-walker (name &body body)
  (with-standard-definer-options name
    `(bind ((hu.dwim.walker::*walker-handlers* *js-walker-handlers*))
       (def walker ,name
         ,@body))))

(def layered-methods find-walker-handler
  (:method :in js :around (form)
    (or (and (consp form)
             (bind (((:values handler found?) (gethash (first form) *js-walker-handlers*)))
               (when found?
                 (assert (not (null handler)))
                 handler)))
        (call-next-layered-method)))
  (:method :in js ((ast-node syntax-node))
    (constantly ast-node)))

(def layered-method hu.dwim.walker::walk-lambda-like :in js (ast-node args body env &rest rest &key &allow-other-keys)
   (apply #'call-next-layered-method ast-node (%fixup-lambda-list args) body env rest))

(def function walk-js (form &optional lexenv)
  (with-active-layers (js)
    (labels ((recurse (x)
               (typecase x
                 (list-quasi-quote (run-transformation-pipeline x))
                 (cons (cons (recurse (car x))
                             (recurse (cdr x))))
                 (t x))))
      ;; let's transform all list qq nodes inside the form (this handles ` in macrolets in js forms)
      (setf form (recurse form)))
    (walk-form form :environment (make-walk-environment lexenv))))


;;;;;;
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

;;;;;;
;;; some js specific handlers

(def function %fixup-lambda-list (args)
  ;; this is kinda hackish, but does what we want
  (nsubstitute '&rest '|&rest|
               (nsubstitute '&optional '|&optional|
                            (nsubstitute '&allow-other-keys '|&allow-other-keys|
                                         (substitute '&key '|&key| args)))))

(def js-walker |defun|
  (bind (((name args &rest body) (rest -form-)))
    (with-form-object (node 'function-definition-form -parent- :name name)
      (walk-lambda-like node args body -environment-))))

;; cl:lambda is a macro that expands to (function (lambda ...)), so we need to define our own handler here
(def (js-walker e) |lambda|
  (walk-form `(function ,-form-) :parent -parent- :environment -environment-))

(def (js-walker e) |return|
  (unless (<= 1 (length -form-) 2)
    (simple-walker-error "Illegal return form: ~S" -form-))
  (bind ((value (second -form-)))
    (with-form-object (return-from-node 'return-from-form -parent-)
      (setf (result-of return-from-node) (when value
                                           (walk-form value :parent return-from-node :environment -environment-))))))

(def class* for-form (walked-form)
  ((variables)
   (steps)
   (looping-condition)
   (body)))

(def (js-walker e) |do|
  (with-form-object (for-node 'for-form -parent-)
    (bind (((raw-variables (raw-end-test &optional result) &rest raw-body) (rest -form-)))
      (when result
        (js-compile-error for-node "DO can't handle a result expression"))
      (setf (values (variables-of for-node) (steps-of for-node))
            (iter (for entry :in raw-variables)
                  (for (var init step) = (ensure-list entry))
                  (collect (make-instance 'setq-form
                                          :parent for-node
                                          :variable (hu.dwim.walker::recurse var for-node)
                                          :value (hu.dwim.walker::recurse init for-node))
                    :into variables)
                  (when step
                    (collect (hu.dwim.walker::recurse `(setq ,var ,step) for-node) :into steps))
                  (finally (return (values variables steps)))))
      (setf (looping-condition-of for-node) (hu.dwim.walker::recurse `(|not| ,raw-end-test) for-node))
      (setf (body-of for-node) (mapcar [hu.dwim.walker::recurse !1 for-node] raw-body)))))

(def class* create-form (walked-form)
  ((elements)))

(def (js-walker e) |create|
  (bind ((elements (rest -form-)))
    (with-form-object (create-node 'create-form -parent-)
      (setf (elements-of create-node) (mapcar [hu.dwim.walker::recurse !1 create-node] elements)))))

(def class* array-form (walked-form)
  ((elements)))

(def (js-walker e) |array|
  (bind ((elements (rest -form-)))
    (with-form-object (toplevel-create-node 'array-form -parent-)
      (labels ((recurse (node)
                 (if (and (vectorp node)
                          (not (stringp node)))
                     (with-form-object (create-node 'array-form -parent-)
                       (setf (elements-of toplevel-create-node) (map 'list #'recurse node)))
                     (hu.dwim.walker::recurse node toplevel-create-node))))
        (setf (elements-of toplevel-create-node) (mapcar #'recurse elements))))))

(def class* slot-value-form (walked-form)
  ((object)
   (slot-name)))

(def (js-walker e) |slot-value|
  (unless (length= 2 (rest -form-))
    (js-compile-error nil "Invalid slot-value form" -form-))
  (with-form-object (node 'slot-value-form -parent-)
    (setf (object-of node) (hu.dwim.walker::recurse (second -form-) node))
    (setf (slot-name-of node) (bind ((slot-name (third -form-)))
                                (if (quoted-symbol? slot-name)
                                    (second slot-name)
                                    (hu.dwim.walker::recurse slot-name node))))))

(def class* instantiate-form (walked-form)
  ((type-to-instantiate)
   (arguments)))

(def (js-walker e) |new|
  (when (< (length -form-) 2)
    (js-compile-error nil "Invalid 'new' form, needs at least two elements: ~S" -form-))
  (bind ((type (second -form-))
         (args (cddr -form-)))
    (with-form-object (node 'instantiate-form -parent-)
      (setf (type-to-instantiate-of node) type)
      (setf (arguments-of node) (mapcar [hu.dwim.walker::recurse !1 node] args)))))

(def class* try-form (walked-form)
  ((protected-form)
   (catch-clauses)
   (finally-clause)))

(def (js-walker e) |try|
  (when (< (length (rest -form-)) 2)
    (js-compile-error nil "Invalid 'try' form, needs at least two elements: ~S" -form-))
  (with-form-object (node 'try-form -parent-)
    (bind ((body (second -form-))
           (catch-clauses (copy-list (rest (rest -form-))))
           (finally-clause (bind ((finally (assoc '|finally| catch-clauses)))
                             (when finally
                               (setf catch-clauses (remove-if [eq (first !1) '|finally|] catch-clauses))
                               (rest finally)))))
      (setf (finally-clause-of node) (mapcar [hu.dwim.walker::recurse !1 node] finally-clause))
      (setf (catch-clauses-of node)  (mapcar [hu.dwim.walker::recurse !1 node] catch-clauses))
      (setf (protected-form-of node) (hu.dwim.walker::recurse body node)))))

(def class* catch-form (implicit-progn-mixin walked-form)
  ((variable-name)
   (condition)))

(def (js-walker e) |catch|
  (when (< (length (rest -form-)) 2)
    (js-compile-error nil "Invalid 'catch' form, needs at least two elements: ~S" -form-))
  (bind (((nil (variable-name &rest condition) &body body) -form-))
    (unless (and variable-name
                 (symbolp variable-name))
      (js-compile-error nil "The condition variable in a 'catch' form must be a symbol. Got ~S instead." variable-name))
    (with-form-object (node 'catch-form -parent-)
      (setf (variable-name-of node) variable-name)
      (setf (condition-of node) (when condition
                                  (hu.dwim.walker::recurse condition node)))
      (setf (hu.dwim.walker:body-of node) (mapcar [hu.dwim.walker::recurse !1 node] body)))))

(def class* while-form (implicit-progn-mixin walked-form)
  ((condition)))

(def (js-walker e) |while|
  (when (< (length (rest -form-)) 2)
    (js-compile-error nil "Invalid 'while' form, needs at least two elements: ~S" -form-))
  (bind (((nil condition &body body) -form-))
    (with-form-object (node 'while-form -parent-)
      (setf (condition-of node) (hu.dwim.walker::recurse condition node))
      (setf (hu.dwim.walker:body-of node) (mapcar [hu.dwim.walker::recurse !1 node] body)))))

(def (js-walker e) |macrolet|
  ;; this is a KLUDGE: the walker only understands &BODY but the js reader is case sensitive
  (funcall (find-walker-handler `(macrolet))
           `(macrolet (,@(iter (for (name args . body) :in (second -form-))
                               (collect `(,name ,(substitute '&key '|&key|
                                                             (substitute '&body '|&body| args))
                                                ,@body))))
              ,@(rest (rest -form-)))
           -parent- -environment-))

(def class* type-of-form (walked-form)
  ((object)))

(def (js-walker e) |type-of|
  (unless (length= 2 -form-)
    (js-compile-error nil "Invalid 'type-of' form, needs exactly one argument: ~S" -form-))
  (bind (((nil object) -form-))
    (with-form-object (node 'type-of-form -parent-)
      (setf (object-of node) (hu.dwim.walker::recurse object node)))))

(def class* regexp-form (walked-form)
  ((regexp)))

(def (js-walker e) |regexp|
  (bind ((regexp (second -form-)))
    (unless (and (length= 2 -form-)
                 (stringp regexp))
      (js-compile-error nil "Invalid 'regexp' form, needs exactly one argument, a string: ~S" -form-))
    (with-form-object (node 'regexp-form -parent-)
      (setf (regexp-of node) regexp))))


;; reinstall some cl handlers on the same, but lowercase symbol exported from hu.dwim.quasi-quote.js
;; because `js is case sensitive...
(progn
  (dolist (symbol {with-preserved-readtable-case
                   ;; NOTE lambda needs its own handler, see above
                   '(progn let let* setf setq defun block return if unwind-protect flet)})
    (export symbol :hu.dwim.quasi-quote.js)
    (bind ((cl-symbol (find-symbol (string-upcase (symbol-name symbol)) :common-lisp)))
      (assert cl-symbol)
      (awhen (gethash cl-symbol hu.dwim.walker::*walker-handlers*)
        (setf (gethash symbol *js-walker-handlers*) it))))

  (macrolet ((js-to-lisp-handler-alias (new existing)
               `(setf (gethash ',new *js-walker-handlers*) (gethash ',existing hu.dwim.walker::*walker-handlers*))))
    (js-to-lisp-handler-alias |setf| setq))

  (macrolet ((js-handler-alias (new existing)
               `(progn
                  (setf (gethash ',new *js-walker-handlers*) (gethash ',existing *js-walker-handlers*))
                  (export ',new))))
    (js-handler-alias |typeof| |type-of|)
    (js-handler-alias type-of  |type-of|)))
