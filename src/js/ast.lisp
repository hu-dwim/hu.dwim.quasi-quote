;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

;; The quasi quoted JavaScript AST is the cl-walker AST plus js-quasi-quote, js-unquote

(def ast js)

;; TODO move these into def ast?
(def class* js-syntax-node (syntax-node)
  ())

(def class* js-quasi-quote (quasi-quote js-syntax-node)
  ())

(def (function e) make-js-quasi-quote (body)
  (make-instance 'js-quasi-quote :body body))

(def class* js-unquote (unquote js-syntax-node)
  ())

(def (function e) make-js-unquote (form &optional (spliced? #f))
  (make-instance 'js-unquote :form form :spliced spliced?))

(def function make-js-indent ()
  (when *js-indent*
    (list (make-spaces (* *js-indent* *js-indent-level*)))))

(def (function o) convert-js-operator-name (op)
  (case op
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(def (function o) lisp-constant-to-js-constant (value)
  ;; TODO
  (princ-to-string value))

(def macro transform-incf-like (node plus-plus plus-equal)
  `(bind ((arguments (arguments-of ,node)))
     (ecase (length arguments)
       (1 `(,(recurse (first arguments)) ,',plus-plus))
       (2 `(,(recurse (first arguments)) " " ,',plus-equal " " ,(recurse (second arguments)))))))

(def macro with-increased-indent (&body body)
  `(bind ((*js-indent-level* (1+ *js-indent-level*)))
    ,@body))

(def macro with-increased-indent* ((really?) &body body)
  `(if ,really?
       (bind ((*js-indent-level* (1+ *js-indent-level*)))
         ,@body)
       (progn
         ,@body)))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name . body) :in entries)
                        (collect `(def js-special-form ,name
                                    ,@body))))))
  (frob
   (incf (transform-incf-like -node- "++" "+="))
   (decf (transform-incf-like -node- "--" "-="))
   (not
     (assert (length= 1 (arguments-of -node-)))
     `("!(" ,(recurse (first (arguments-of -node-))) ")"))))

(def function transform-progn (node)
  (bind ((body (cl-walker:body-of node))
         (wrap? (if (rest body)
                    #t
                    #f)))
    `(,@(when wrap? (list #\{))
      ,@(with-increased-indent* (wrap?)
         (iter (for statement :in body)
               (when wrap?
                 (collect #\Newline))
               (awhen (make-js-indent)
                 (collect it))
               (collect (transform-quasi-quoted-js-to-quasi-quoted-string statement))
               (collect #\;)))
      ,@(when wrap? (list #\Newline #\})))))

(macrolet ((frob (&rest entries)
             `(flet ((recurse (form)
                       (transform-quasi-quoted-js-to-quasi-quoted-string form)))
                (defgeneric transform-quasi-quoted-js-to-quasi-quoted-string* (form)
                  ,@(iter (for (type . body) :in entries)
                          (collect `(:method ((-node- ,type))
                                      ,@body)))))))
  (frob
   (variable-reference-form
    (symbol-name (name-of -node-)))
   (implicit-progn-mixin
    (transform-progn -node-))
   (application-form
    (bind ((operator (operator-of -node-))
           (operator-name (symbol-name (convert-js-operator-name operator))))
      (cond
        ((js-special-form? operator)
         (bind ((handler (gethash operator *js-special-forms*)))
           (funcall handler -node-)))
        (t
         `(,operator-name #\(
                          ,@(mapcar #'recurse (arguments-of -node-))
                          #\) )))))
   (constant-form
    (lisp-constant-to-js-constant (value-of -node-)))))
