;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

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

(def class* syntax-node ()
  ())

(eval-when (:compile-toplevel :load-toplevel)
  (def class* void-syntax-node ()
    ()))

(def function void-syntax-node= (node1 node2)
  (and (typep node1 'void-syntax-node)
       (typep node2 'void-syntax-node)))

(def (constant :test void-syntax-node=) +void-syntax-node+ (make-instance 'void-syntax-node))

(def (class* e) quasi-quote (syntax-node)
  ((body)))

(def (class* e) unquote (syntax-node)
  ((form)
   (spliced #f :type boolean)))

(def function ast-package (name)
  (bind ((package (symbol-package name)))
    (if (eq package (find-package :common-lisp))
        (find-package :cl-quasi-quote)
        package)))

(def definer ast (name)
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
                    (,name "-EMITTING-FORM")
                    (,name "-EMITTING-LAMBDA-FORM")
                    (,name "-EMITTING-LAMBDA")
                    ("QUOTED-" ,name)
                    ("QUOTED-" ,name "-EMITTING-FORM")
                    ("QUOTED-" ,name "-EMITTING-LAMBDA-FORM")
                    ("QUOTED-" ,name "-EMITTING-LAMBDA")))
        ,package))))

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

(defgeneric transform (to from &key &allow-other-keys)
  (:method (to from &key &allow-other-keys)
    (error "Don't know how to transform ~A to ~A" from to))

  (:method ((to list) from &key &allow-other-keys)
    (apply #'transform (first to) from (cdr to)))

  (:method ((to symbol) from &rest args &key &allow-other-keys)
    (bind ((name (symbol-name to))
           (package (ast-package to)))
      (cond ((ends-with-subseq "-EMITTING-LAMBDA-FORM" name)
             (bind ((to (format-symbol package "~A-EMITTING-FORM" (subseq name 0 (- (length name) (length "-EMITTING-LAMBDA-FORM"))))))
               `(lambda () ,(apply #'transform to from args))))
            ((ends-with-subseq "-EMITTING-LAMBDA" name)
             (bind ((to (format-symbol package "~A-EMITTING-FORM" name)))
               (compile nil (apply #'transform to from args))))
            ((ends-with-subseq "-EMITTING-FORM" name)
             (syntax-node-emitting-form from)
             #+nil
             `(,(format-symbol package "MAKE-~A-QUASI-QUOTE" (subseq name 0 (- (length name) (length "-EMITTING-FORM"))))
                ,(syntax-node-emitting-form from)))
            ((not (search "QUASI-QUOTED-" name))
             (funcall (apply #'transform (format-symbol package "~A-EMITTING-LAMBDA" name) from args)))
            ((and (starts-with-subseq "QUASI-QUOTED-" name)
                  (typep from (format-symbol package "~A-SYNTAX-NODE"
                                             (subseq name (length "QUASI-QUITED-")))))
             
             from)
            (t (call-next-method)))))

  (:method ((to (eql :lambda)) from &rest args &key &allow-other-keys)
    (compile nil (apply #'transform :lambda-form from args))))

(export 'transform)

(def (function e) chain-transform (through from)
  (iter (for node :initially from :then (transform element node))
        (for element :in through)
        (finally (return node))))

(defgeneric syntax-node-emitting-form (node)
  (:method ((node symbol))
    node)

  (:method ((node number))
    node)

  (:method ((node string))
    node)

  (:method ((node list))
    (iter (for element :in node)
          (collect (when (typep element 'unquote)
                     (spliced-p element)) :into spliced-elements)
          (collect (syntax-node-emitting-form element) :into transformed-elements)
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
    (syntax-node-emitting-form (body-of node)))

  (:method ((node unquote))
    (map-filtered-tree (form-of node) 'quasi-quote #'syntax-node-emitting-form))

  (:method ((node syntax-node))
    (bind ((class (class-of node)))
      `(make-instance ',(class-name class)
                      ,@(iter (for slot :in (class-slots class))
                              (when (slot-boundp-using-class class node slot)
                                (appending (list (first (slot-definition-initargs slot))
                                                 (syntax-node-emitting-form (slot-value-using-class class node slot))))))))))

(export 'syntax-node-emitting-form)

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

(def (function e) map-tree (form map-function)
  (labels ((process (form)
             (cond ((null form)
                    nil)
                   ((consp form)
                    (cons (process (car form))
                          (process (cdr form))))
                   (t (funcall map-function form)))))
    (process form)))

(def (function e) map-filtered-tree (form type map-function)
  (map-tree form
            (lambda (form)
              (if (typep form type)
                  (funcall map-function form)
                  form))))

(def function name-as-string (name)
  (etypecase name
    (string name)
    (symbol (string-downcase name))))
