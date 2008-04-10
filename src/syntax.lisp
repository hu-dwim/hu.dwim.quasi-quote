;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

;; TODO there's a *quasi-quote-level* in syntax-sugar, too. cleanup!
(def special-variable *quasi-quote-level* 0)

(def function readtime-chain-transform (to from)
  (if (= 1 *quasi-quote-level*)
      (chain-transform to from)
      from))

(def (function e) with-transformed-quasi-quoted-syntax (&rest transforms)
  (lambda (reader)
    (bind (((name &rest args) (ensure-list (first transforms))))
      (chain-transform (cdr transforms)
                       (funcall (apply (format-symbol *package*  "WITH-~A-SYNTAX" name) args)
                                reader)))))

;;;;;;;
;;; AST

;; TODO: +void+ should be replaced by using (values) in user code (somewhat difficult)
(def (constant e :test (lambda (o1 o2) (eq (type-of o1) (type-of o2)))) +void+ (lambda ()))

(def class* syntax-node ()
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

(def (class* e) parent-mixin ()
  ((parent :type syntax-node)))

(def constructor parent-mixin
  (iter (with class = (class-of self))
        (for slot :in (class-slots class))
        (when (slot-boundp-using-class class self slot)
          (bind ((value (slot-value-using-class class self slot)))
            (typecase value
              (parent-mixin (setf (parent-of value) self))
              (list (when (eq 'list (slot-definition-type slot))
                      (dolist (element value)
                        (setf (parent-of element) self)))))))))

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

(def function wrap-forms-with-lambda (forms &optional optimize)
  `(lambda ()
     ,@(when optimize
             `((declare (optimize ,@optimize))))
     ,@(if (and (consp forms)
                (eq 'progn (first forms)))
           (cdr forms)
           forms)))

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

(def (function e) chain-transform (through from)
  (iter (for node :initially from :then (transform element node))
        (for element :in through)
        (finally (return node))))

(defgeneric make-syntax-node-emitting-form (node)
  (:method ((node symbol))
    node)

  (:method ((node number))
    node)

  (:method ((node string))
    node)

  (:method ((node hash-table))
    (with-unique-names (table)
      `(bind ((,table (make-hash-table :test ',(hash-table-test node))))
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
                      ,@(iter (for slot :in (class-slots class))
                              (when (and (not (eq 'parent (slot-definition-name slot)))
                                         (slot-boundp-using-class class node slot))
                                (appending (list (first (slot-definition-initargs slot))
                                                 (make-syntax-node-emitting-form (slot-value-using-class class node slot))))))))))

(export 'make-syntax-node-emitting-form)

;;;;;;;;
;;; Emit

(defgeneric setup-emitting-environment (to &key &allow-other-keys)
  (:method (to &key next-method &allow-other-keys)
    (funcall next-method))

  (:method ((to cons) &rest args &key &allow-other-keys)
    (apply #'setup-emitting-environment (first to) (append (cdr to) args))))

(export 'setup-emitting-environment)

(def (function e) emit (through from)
  (iter (for thunk
             :initially (lambda ()
                          (etypecase from
                            (syntax-node (body-of from))
                            (function (funcall from))))
             :then (bind ((thunk thunk)
                          (element element))
                     (lambda ()
                       (setup-emitting-environment element :next-method thunk))))
        (for element :in (reverse through))
        (finally (return (funcall thunk)))))

;;;;;;;;
;;; Util

(def (function o) vector-extend (extension vector)
  (bind ((original-length (length vector))
         (extension-length (length extension))
         (new-length (+ original-length extension-length))
         (original-dimension (array-dimension vector 0)))
    (when (< original-dimension new-length)
      (setf vector (adjust-array vector (max (* 2 original-dimension) new-length))))
    (setf (fill-pointer vector) new-length)
    (replace vector extension :start1 original-length)
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
