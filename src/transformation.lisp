;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;; TODO delme
(def (special-variable e) *quasi-quote-stream*)

(def special-variable *transformation*)

(def (class* e) transformation ()
  ((handler :type (or symbol function))))

(def generic compatible-transformations? (a b)
  (:method (a b)
    #f))

(def function compatible-transformation-pipelines? (a b)
  (every (lambda (a b)
           (compatible-transformations? a b))
         a b))

(def class* emitting-form-transformation (transformation)
  ((with-inline-emitting #f :accessor with-inline-emitting? :documentation "WITH-INLINE-EMITTING means that the order of the creation of the syntax nodes at runtime is in sync with the expected order of these nodes in the output (i.e. nothing like <a () ,@(reverse (list <b> <c>))>). It enables an optimization: in this mode the write-sequence calls are not wrapped in closures but rather everything is emitted at the place where it is in the code.")
   (stream-variable-name)
   (declarations '() :documentation "Add these declarations to the emitted lambda forms.")))

(def method compatible-transformations? ((a emitting-form-transformation)
                                         (b emitting-form-transformation))
  (or (eq a b)
      (and (eq (class-of a) (class-of b))
           (eql (with-inline-emitting? a) (with-inline-emitting? b))
           (eql (stream-variable-name-of a) (stream-variable-name-of b))
           (equalp (declarations-of a) (declarations-of b)))))

(def function ensure-progn (forms)
  (if (and (consp forms)
           (eq 'progn (first forms)))
      forms
      `(progn ,@forms)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def class* delayed-emitting ()
    ()
    (:metaclass funcallable-standard-class)
    (:documentation "A distinct type to be able to assert for it while emitting.")))

;; TODO: +void+ should be replaced by using (values) in user code (somewhat difficult)
(def (constant e :test (constantly #t)) +void+ (bind ((result (make-instance 'delayed-emitting)))
                                                 (set-funcallable-instance-function result (lambda () (values)))
                                                 result))

(def (function i) make-delayed-emitting (thunk)
  (bind ((result (make-instance 'delayed-emitting)))
    (set-funcallable-instance-function result thunk)
    result))

(def function wrap-emitting-forms (with-inline-emitting forms &optional declarations)
  (bind ((forms
          (append forms
                  (if with-inline-emitting
                      '(+void+)
                      '((values))))))
    (if with-inline-emitting
        (ensure-progn forms)
        `(make-delayed-emitting (lambda ()
                                  ,@declarations
                                  ,@(if (and (consp forms)
                                             (consp (first forms))
                                             (eq 'progn (first (first forms))))
                                        (cdr forms)
                                        forms))))))

(def function wrap-forms-with-bindings (bindings forms)
  (if bindings
      `(bind ,bindings
         ,forms)
      forms))

(def (function e) run-transformation-pipeline (node)
  (assert (typep node 'quasi-quote))
  (bind ((transformation-pipeline (transformation-pipeline-of node))
         (*transformation* nil))
    (iter (for transformation :in-sequence transformation-pipeline)
          (setf *transformation* transformation)
          (setf node (funcall (handler-of transformation) node))))
  node)

(def generic make-syntax-node-emitting-form (node)
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

(defgeneric collect-slots-for-syntax-node-emitting-form (node)
  (:method ((node syntax-node))
    (remove 'parent (class-slots (class-of node)) :key #'slot-definition-name)))

(export 'collect-slots-for-syntax-node-emitting-form)

;;;;;;;;
;;; Emit

(def (macro e) emit (ast)
  (once-only (ast)
    `(progn
       (assert (or (eq ,ast +void+)
                   (typep ,ast 'delayed-emitting))
               () "Something went awry around the quasi quoted stuff, EMIT got a ~S." ,ast)
       (funcall ,ast))))

#+nil ; TODO delme
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
