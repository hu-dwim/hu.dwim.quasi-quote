;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def special-variable *transformation-pipeline*)
(def special-variable *transformation*)
(def special-variable *disable-run-transformation-pipeline* #f
  "For debugging purposes. When T then RUN-TRANSFORMATION-PIPELINE simply returns its argument.")

(def function wrap-runtime-delayed-transformation-form (form)
  `(bind ((*transformation* ,*transformation*))
     ,form))

(def (class* e) transformation ()
  ((transformer :type (or symbol function))))

(def (definer e :available-flags "e") transformation (name supers slots transformer &body class-options)
  `(progn
     (def (class* ,@(when (getf -options- :export) '(:export t))) ,name ,(if (find 'transformation supers)
                              supers
                              (append supers '(transformation)))
       ,slots
       ,@class-options)
     (def method initialize-instance :after ((self ,name) &key)
       (bind ((-transformation- self))
         (declare (ignorable -transformation-))
         (setf (transformer-of self) ,transformer)))))

(def method make-load-form ((self transformation) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(def generic compatible-transformations? (a b)
  (:method-combination and)
  (:method and (a b)
    #t)
  (:method :around (a b)
    (or (eq a b)
        (and (or (typep a (type-of b))
                 (typep b (type-of a)))
             (call-next-method)))))

(def function compatible-transformation-pipelines? (a b)
  (every (lambda (a b)
           (compatible-transformations? a b))
         a b))

(def class* lisp-form-emitting-transformation (transformation)
  ((with-inline-emitting #f :accessor with-inline-emitting? :documentation "WITH-INLINE-EMITTING means that the order of the creation of the syntax nodes at runtime is in sync with the expected order of these nodes in the output (i.e. nothing like <a () ,@(reverse (list <b> <c>))>). It enables an optimization: in this mode the write-sequence calls are not wrapped in closures but rather everything is emitted at the place where it is in the code.")
   ;; TODO there's some confusion here: stream-variable-name and declarations is meaningless for quasi-quoted-list-to-list-emitting-form...
   (stream-variable-name)
   (declarations '() :documentation "Add these declarations to the emitted lambda forms.")))

(def method compatible-transformations? and ((a lisp-form-emitting-transformation)
                                             (b lisp-form-emitting-transformation))
  (and (eql (with-inline-emitting? a) (with-inline-emitting? b))
       (eql (stream-variable-name-of a) (stream-variable-name-of b))
       (equalp (declarations-of a) (declarations-of b))))

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
(def (special-variable e) +void+ (bind ((result (make-instance 'delayed-emitting)))
                                   (set-funcallable-instance-function result (lambda () (values)))
                                   result))

(def (function i) make-delayed-emitting (thunk)
  (bind ((result (make-instance 'delayed-emitting)))
    (set-funcallable-instance-function result thunk)
    result))

(def macro as-delayed-emitting (&body body)
  `(make-delayed-emitting (lambda () ,@body)))

(def function wrap-emitting-forms (forms)
  (bind ((with-inline-emitting (with-inline-emitting? *transformation*))
         (forms (if (eq with-inline-emitting :as-is)
                    forms
                    (append forms
                            (if with-inline-emitting
                                '(+void+)
                                '((values)))))))
    (if with-inline-emitting
        (ensure-progn forms)
        `(as-delayed-emitting
           ,@(declarations-of *transformation*)
           ,@(if (and (consp forms)
                      (eq 'progn (first forms)))
                 (cdr forms)
                 forms)))))

(def function wrap-forms-with-bindings (bindings forms)
  (if bindings
      `(bind ,bindings
         ,forms)
      forms))

(def function run-transformation-pipeline (node)
  (assert (typep node 'quasi-quote))
  (unless *disable-run-transformation-pipeline*
    (bind ((*transformation-pipeline* (transformation-pipeline-of node)))
      (iter (setf node (transform node))
            (while (typep node 'quasi-quote)))))
  node)

(def function transform (node)
  (assert (typep node 'quasi-quote))
  (bind ((*transformation* (first (transformation-pipeline-of node))))
    (funcall (transformer-of *transformation*) node)))

(def macro transformation-typecase (quasi-quote-node &body cases)
  (once-only (quasi-quote-node)
    `(etypecase ,quasi-quote-node
       ,@cases
       (quasi-quote (transform ,quasi-quote-node))
       (delayed-emitting ,quasi-quote-node)
       (side-effect ,quasi-quote-node))))

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
    (map-filtered-tree (form-of node) 'quasi-quote #'run-transformation-pipeline))

  (:method ((node syntax-node))
    (bind ((class (class-of node)))
      `(make-instance ',(class-name class)
                      ,@(iter (for slot :in (collect-slots-for-syntax-node-emitting-form node))
                              (when (slot-boundp-using-class class node slot)
                                (appending (list (first (slot-definition-initargs slot))
                                                 (make-syntax-node-emitting-form (slot-value-using-class class node slot))))))))))

(def generic collect-slots-for-syntax-node-emitting-form (node)
  (:method ((node syntax-node))
    (remove 'parent (class-slots (class-of node)) :key #'slot-definition-name)))

(def (transformation e) quasi-quoted-syntax-node-to-syntax-node-emitting-form (lisp-form-emitting-transformation)
  ()
  'make-syntax-node-emitting-form)

;;;;;;;;
;;; Emit

(def (macro e) emit (ast)
  (once-only (ast)
    `(progn
       (assert (typep ,ast 'delayed-emitting) () "Something went awry around the quasi quoted stuff, EMIT got a ~S." ,ast)
       (funcall ,ast))))

