;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def function transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'js-quasi-quote fn))

(def special-variable *js-indent* 0)
(def special-variable *js-indent-level* 0)

(def function make-js-indent ()
  (when *js-indent*
    (list (make-spaces (* *js-indent* *js-indent-level*)))))

(def macro with-increased-indent (&body body)
  `(bind ((*js-indent-level* (1+ *js-indent-level*)))
     ,@body))

(def macro with-increased-indent* ((really?) &body body)
  `(if ,really?
       (bind ((*js-indent-level* (1+ *js-indent-level*)))
         ,@body)
       (progn
         ,@body)))

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

(def function transform-quasi-quoted-js-to-quasi-quoted-string (node)
  (etypecase node
    (function       node)
    (string         (bind ((indent (when *js-indent*
                                     (list (make-spaces (* *js-indent* *js-indent-level*))))))
                      `(,@indent ,(escape-as-js node) #\NewLine)))
    (integer        (princ-to-string node))
    (form           (transform-quasi-quoted-js-to-quasi-quoted-string* node))
    (js-quasi-quote (make-string-quasi-quote (transform-quasi-quoted-js-to-quasi-quoted-string (body-of node))))
    (js-unquote     (transform-quasi-quoted-js-to-quasi-quoted-string/unquote node))
    ;; TODO ?
    (quasi-quote    (if (typep node 'string-quasi-quote)
                        (body-of node)
                        node))
    (unquote        (transform 'quasi-quoted-string node))
    (side-effect    node)))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/unquote (node)
  (assert (typep node 'js-unquote))
  (bind ((spliced? (spliced-p node)))
    ;; TODO bullshit copy-paste...
    (make-string-unquote
     (if spliced?
         `(map 'list (lambda (node)
                       ,(wrap-forms-with-bindings
                         (when *js-indent* `((*js-indent-level* ,*js-indent-level*)))
                         `(transform-quasi-quoted-js-to-quasi-quoted-string node)))
               ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
                 node (lambda (node)
                        (transform-quasi-quoted-js-to-quasi-quoted-string node))))
         (wrap-forms-with-bindings
          (when *js-indent* `((*js-indent-level* ,*js-indent-level*)))
          `(transform-quasi-quoted-js-to-quasi-quoted-string
            ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
              node (lambda (node)
                     (transform-quasi-quoted-js-to-quasi-quoted-string node))))))
     spliced?)))

(def method transform ((to (eql 'quasi-quoted-string)) (input js-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-js-to-quasi-quoted-string input args))
