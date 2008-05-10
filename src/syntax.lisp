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

;;;;;;;;;
;;; Parse

(def function readtime-chain-transform (transformatation from)
  (if (= 1 *quasi-quote-nesting-level*)
      (chain-transform transformatation from)
      from))

(def (function e) with-transformed-quasi-quoted-syntax (&rest transformatations)
  (lambda (reader)
    (bind (((name &rest args) (ensure-list (first transformatations))))
      (chain-transform (cdr transformatations)
                       (funcall (apply (format-symbol (symbol-package name)  "WITH-~A-SYNTAX" name) args)
                                reader)))))

;;;;;;;
;;; AST

;; TODO: +void+ should be replaced by using (values) in user code (somewhat difficult)
(def (constant e :test (lambda (o1 o2) (eq (type-of o1) (type-of o2)))) +void+ (lambda () (values)))

(def (class* e) syntax-node ()
  ())

(def (class* e) quasi-quote (syntax-node)
  ((body)))

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

;;;;;;;;;;;;;
;;; Transform

(def (special-variable e) *quasi-quote-stream*)

(def function wrap-forms-with-progn (forms)
  (if (and (consp forms)
           (eq 'progn (first forms)))
      forms
      `(progn ,@forms)))

(def function wrap-forms-with-lambda (forms &optional optimize)
  `(lambda ()
     ,@(when optimize
             `((declare (optimize ,@optimize))))
     ,@(if (and (consp forms)
                (consp (first forms))
                (eq 'progn (first (first forms))))
           (cdr forms)
           forms)))

(def function wrap-emitting-forms (properly-ordered forms)
  (bind ((forms
          (append forms
                  (if properly-ordered
                      '(+void+)
                      '((values))))))
    (if properly-ordered
        (wrap-forms-with-progn forms)
        (wrap-forms-with-lambda forms))))

(def function wrap-forms-with-bindings (bindings forms)
  (if bindings
      `(bind ,bindings
         ,forms)
      forms))

(defgeneric transform (to from &key &allow-other-keys)
  (:method (to from &key &allow-other-keys)
    (error "Don't know how to transform ~A to ~A" from to))

  (:method ((to cons) from &key &allow-other-keys)
    (apply #'transform (first to) from (cdr to)))

  (:method ((to symbol) from &key &allow-other-keys)
    (bind ((name (symbol-name to))
           (package (ast-package to)))
      (cond ((ends-with-subseq "-EMITTING-FORM" name)
             ;; call generic form emitter for CLOS AST 
             (make-syntax-node-emitting-form from))
            ((and (not (search "QUASI-QUOTED-" name))
                  (typep from (format-symbol package "~A-SYNTAX-NODE" name)))
             (funcall (chain-transform `(,(format-symbol package "~A-EMITTING-FORM" name) lambda-form lambda) from)))
            ;; when FROM is actually an instance of TO
            ((and (starts-with-subseq "QUASI-QUOTED-" name)
                  (typep from (format-symbol package "~A-SYNTAX-NODE"
                                             (subseq name (length "QUASI-QUITED-")))))
             from)
            ;; call the specific transformation
            (t (call-next-method)))))

  (:method ((to (eql 'lambda-form)) (from cons) &key optimize &allow-other-keys)
    (wrap-forms-with-lambda (list from) optimize))

  (:method ((to (eql 'lambda)) (from cons) &key &allow-other-keys)
    (compile nil from)))

(export 'transform)

(def (function e) chain-transform (transformation from)
  (iter (for node :initially from :then (transform element node))
        (for element :in transformation)
        (finally (return node))))

(defgeneric make-syntax-node-emitting-form (node)
  (:method ((node null))
    nil)

  (:method ((node symbol))
    (if (keywordp node)
        node
        (list 'quote node)))

  (:method ((node number))
    node)

  (:method ((node string))
    node)

  (:method ((node function))
    node)

  (:method ((node hash-table))
    (with-unique-names (table)
      `(prog1-bind ,table (make-hash-table :test ',(hash-table-test node))
         ,@(iter (for (key value) :in-hashtable node)
                 (collect `(setf (gethash ,(make-syntax-node-emitting-form key) ,table)
                                 ,(make-syntax-node-emitting-form value)))))))

  (:method ((node list))
    (iter (for element :in node)
          (collect (when (typep element 'unquote)
                     (spliced-p element)) :into spliced-elements)
          (collect (make-syntax-node-emitting-form element) :into transformed-elements)
          (finally (return
                     (cond ((every #'identity spliced-elements)
                            `(append ,@transformed-elements))
                           ((notany #'identity spliced-elements)
                            `(list ,@transformed-elements))
                           (t `(append ,@(mapcar (lambda (spliced element)
                                                   (if spliced
                                                       element
                                                       `(list ,element)))
                                                 spliced-elements transformed-elements))))))))

  (:method ((node quasi-quote))
    (make-syntax-node-emitting-form (body-of node)))

  (:method ((node unquote))
    (map-filtered-tree (form-of node) 'quasi-quote #'make-syntax-node-emitting-form))

  (:method ((node syntax-node))
    (bind ((class (class-of node)))
      `(make-instance ',(class-name class)
                      ,@(iter (for slot :in (collect-slots-for-syntax-node-emitting-form node))
                              (when (slot-boundp-using-class class node slot)
                                (appending (list (first (slot-definition-initargs slot))
                                                 (make-syntax-node-emitting-form (slot-value-using-class class node slot))))))))))

(export 'make-syntax-node-emitting-form)

(defgeneric collect-slots-for-syntax-node-emitting-form (node)
  (:method ((node syntax-node))
    (remove 'parent (class-slots (class-of node)) :key #'slot-definition-name)))

(export 'collect-slots-for-syntax-node-emitting-form)

;;;;;;;;
;;; Emit

(defgeneric setup-emitting-environment (to &key &allow-other-keys)
  (:method (to &key next-method &allow-other-keys)
    (funcall next-method))

  (:method ((to cons) &rest args &key &allow-other-keys)
    (apply #'setup-emitting-environment (first to) (append (cdr to) args))))

(export 'setup-emitting-environment)

(def (macro e) emit (transformation ast)
  `(emit-thunk ,transformation (lambda () ,ast)))

(def function emit-thunk (transformation ast-emitting-thunk)
  (iter (for thunk
             :initially (lambda ()
                          (bind ((ast (funcall ast-emitting-thunk)))
                            (etypecase ast
                              (syntax-node (body-of ast))
                              (function (funcall ast)))))
             :then (bind ((thunk thunk)
                          (element element))
                     (lambda ()
                       (setup-emitting-environment element :next-method thunk))))
        (for element :in (reverse transformation))
        (finally (return (funcall thunk)))))

;;;;;;;;
;;; Util

(def (function o) vector-extend (extension vector &key (start 0) (end (length extension)))
  (declare (type array-index start end))
  (bind ((original-length (length vector))
         (extension-length (- end start))
         (new-length (+ original-length extension-length))
         (original-dimension (array-dimension vector 0)))
    (when (< original-dimension new-length)
      (setf vector (adjust-array vector (max (* 2 original-dimension) new-length))))
    (setf (fill-pointer vector) new-length)
    (replace vector extension :start1 original-length :start2 start :end2 end)
    vector))

(def function reduce-subsequences (sequence predicate reducer)
  (iter (with completely-reduced? = #t)
        (with length = (length sequence))
        (for index :from 0 :below length)
        (for reducibles = (iter (while (< index length))
                                (for element = (elt sequence index))
                                (while (funcall predicate element))
                                (collect element)
                                (incf index)))
        (collect (if (zerop (length reducibles))
                     (progn
                       (setf completely-reduced? #f)
                       (elt sequence index))
                     (progn
                       (decf index)
                       (funcall reducer reducibles)))
          :into result)
        (finally (return (values result completely-reduced?)))))

(def (function e) map-tree (form map-function &optional (process-cons #f))
  (labels ((process (form)
             (cond ((null form)
                    nil)
                   ((consp form)
                    (bind ((result
                            (cons (process (car form))
                                  (process (cdr form)))))
                      (if process-cons
                          (funcall map-function result)
                          result)))
                   (t (funcall map-function form)))))
    (process form)))

(def (function e) map-filtered-tree (form type map-function)
  (map-tree form
            (lambda (form)
              (if (typep form type)
                  (funcall map-function form)
                  form))))

(def (function io) make-spaces (count)
  (make-string count :initial-element #\Space))
