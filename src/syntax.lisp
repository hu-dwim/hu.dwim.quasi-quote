;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

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

(def (class* e) quasi-quote (syntax-node)
  ((body)))

(def (class* e) unquote (syntax-node)
  ((form)
   (spliced #f :type boolean)))

(def definer ast (name)
  (flet ((format-name (arg1 arg2)
           (format-symbol *package* "~A~A" arg1 arg2)))
    `(export ',(list name (format-name "QUASI-QUOTED-" name) (format-name name "-EMITTING-FORM") (format-name name "-EMITTING-LAMBDA-FORM") (format-name name "-EMITTING-LAMBDA")))))

(def method make-load-form ((self syntax-node) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;;;;;;;;;;;;
;;; Transform

(defgeneric transform (to from &key &allow-other-keys)
  (:method (to from &key &allow-other-keys)
    (error "Cannot transform ~A to ~A" from to))

  (:method ((to list) from &key &allow-other-keys)
    (apply #'transform (first to) from (cdr to)))

  (:method ((to symbol) from &rest args &key &allow-other-keys)
    (bind ((name (symbol-name to)))
      (cond ((ends-with-subseq "LAMBDA-FORM" name)
             (bind ((to (format-symbol *package* "~A-FORM" (subseq name 0 (- (length name) 12)))))
               `(lambda () ,(apply #'transform to from args))))
            ((ends-with-subseq "LAMBDA" name)
             (bind ((to (format-symbol *package* "~A-FORM" name)))
               (compile nil (apply #'transform to from args))))
            (t (call-next-method)))))

  (:method ((to (eql :lambda)) from &rest args &key &allow-other-keys)
    (compile nil (apply #'transform :lambda-form from args))))

(export 'transform)

(def (function e) chain-transform (through from)
  (iter (for node :initially from :then (transform element node))
        (for element :in through)
        (finally (return node))))

;;;;;;;;
;;; Util

(def (function e) map-tree (form map-function)
  (labels ((process (form)
             (cond ((null form)
                    nil)
                   ((consp form)
                    (cons (process (car form))
                          (process (cdr form))))
                   (t (funcall map-function form)))))
    (process form)))
