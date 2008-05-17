;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def function transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'js-quasi-quote fn))

(def (special-variable e) *js-indent* 0) ; indenting is off by default
(def special-variable *js-indent-level* 0)

(def function make-js-indent ()
  (when *js-indent*
    (list (make-string-of-spaces (* *js-indent* *js-indent-level*)))))

(def with-macro with-increased-indent* (really?)
  (if really?
      (bind ((*js-indent-level* (1+ *js-indent-level*)))
        (-body-))
      (-body-)))

(def with-macro with-increased-indent ()
  (with-increased-indent* ((not (in-toplevel-js-block?)))
    (-body-)))

(def (function io) lisp-name-to-js-name (symbol)
  (etypecase symbol
    (js-unquote
     (make-string-unquote `(lisp-name-to-js-name ,(form-of symbol))))
    (symbol
     (bind ((name (symbol-name symbol))
            (pieces (cl-ppcre:split "-" name)))
       (if (rest pieces)
           (bind ((*print-pretty* #f))
             (with-output-to-string (str)
               (iter (for piece :in pieces)
                     (write-string (if (first-time-p)
                                       piece
                                       (capitalize-first-letter! piece))
                                   str)
                     (collect piece))))
           name)))))

(def (function o) lisp-operator-name-to-js-operator-name (op)
  (case op
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(def (function oi) lisp-literal-to-js-literal (value)
  (etypecase value
    (string (concatenate 'string "'" (escape-as-js-string value) "'"))
    (integer (princ-to-string value))
    (float (format nil "~F" value))
    (ratio (concatenate 'string "(" (princ-to-string (numerator value)) " / " (princ-to-string (denominator value)) ")"))
    (character (lisp-literal-to-js-literal (string value)))))

(def macro transform-incf-like (node plus-plus plus-equal)
  `(bind ((arguments (arguments-of ,node)))
     (ecase (length arguments)
       (1 `(,',plus-plus ,(recurse (first arguments))))
       (2 `(,(recurse (first arguments)) " " ,',plus-equal " " ,(recurse (second arguments)))))))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name . body) :in entries)
                        (collect `(def js-special-form ,name
                                    ,@body))))))
  (frob
   (|incf| (transform-incf-like -node- "++" "+="))
   (|decf| (transform-incf-like -node- "--" "-="))
   (not
     (assert (length= 1 (arguments-of -node-)))
     `("!(" ,(recurse (first (arguments-of -node-))) ")"))))

(def special-variable *js-block-nesting-level* 0)

(def (function i) in-toplevel-js-block? ()
  (<= *js-block-nesting-level* 1))

(def with-macro within-nested-js-block* (really?)
  (if really?
      (bind ((*js-block-nesting-level* (1+ *js-block-nesting-level*)))
        (-body-))
      (-body-)))

(def with-macro within-nested-js-block ()
  (within-nested-js-block* (#t)
    (-body-)))

(def function transform-progn (node &key (wrap? nil wrap-provided?) (nest? #t) (increase-indent? #f))
  (within-nested-js-block* (nest?)
    (bind ((body (cl-walker:body-of node)))
      (unless wrap-provided?
        (setf wrap? (and (rest body)
                         (not (in-toplevel-js-block?)))))
      `(,@(when wrap? (list #\{))
        ,@(with-increased-indent* (increase-indent?)
            (iter (for statement :in body)
                  (collect #\Newline)
                  (awhen (make-js-indent)
                    (collect it))
                  (collect (transform-quasi-quoted-js-to-quasi-quoted-string statement))
                  (collect #\;)))
        ,@(when wrap? `(#\Newline ,@(make-js-indent) #\}))))))

(def generic transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument (node)
  (:method ((node required-function-argument-form))
    (lisp-name-to-js-name (name-of node))))

(macrolet ((frob (&rest entries)
             `(flet ((recurse (form)
                       (transform-quasi-quoted-js-to-quasi-quoted-string form)))
                (defgeneric transform-quasi-quoted-js-to-quasi-quoted-string* (form)
                  ,@(iter (for (type . body) :in entries)
                          (collect `(:method ((-node- ,type))
                                      ,@body)))))))
  (frob
   (variable-reference-form
    (lisp-name-to-js-name (name-of -node-)))
   (progn-form
    (transform-progn -node-))
   (application-form
    (bind ((operator (operator-of -node-))
           (operator-name (lisp-name-to-js-name (lisp-operator-name-to-js-operator-name operator))))
      (cond
        ((js-special-form? operator)
         (bind ((handler (gethash operator *js-special-forms*)))
           (funcall handler -node-)))
        ((js-operator-name? operator)
         `("("
           ,@(iter (for el :in (arguments-of -node-))
                   (unless (first-time-p)
                     (collect " ")
                     (collect operator-name)
                     (collect " "))
                   (collect (recurse el)))
           ")"))
        (t
         `(,operator-name #\(
                          ,@(mapcar #'recurse (arguments-of -node-))
                          #\) )))))
   (constant-form
    (lisp-literal-to-js-literal (value-of -node-)))
   (variable-binding-form
    (bind ((indent (make-js-indent)))
      (within-nested-js-block
        (with-increased-indent
          `(,@(unless (in-toplevel-js-block?) (list "{"))
            ,@(iter (for (name . value) :in (bindings-of -node-))
                    (collect `(#\Newline ,@(make-js-indent) ,(lisp-name-to-js-name name) " = " ,(recurse value) ";")))
            ,@(transform-progn -node- :wrap? #f :nest? #f :increase-indent? #f)
            ,@(unless (in-toplevel-js-block?) `(#\Newline ,@indent "}")))))))
   (setq-form
    `(,(recurse (variable-of -node-)) " = " ,(recurse (value-of -node-))))
   (function-definition-form
    `("function " ,(lisp-name-to-js-name (name-of -node-))
                  "("
                  ,@(iter (for argument :in (arguments-of -node-))
                          (collect (transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument argument)))
                  ") {"
                  ,@(transform-progn -node- :wrap? #f)
                  #\Newline
                  "}"))
   (return-from-form
    `("return" ,@(awhen (result-of -node-)
                        (list #\space (recurse it)))))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string (node)
  (etypecase node
    (function       node)
    ((or integer float ratio) (lisp-literal-to-js-literal node))
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
          (when *js-indent*
            `((*js-indent* (+ *js-indent* ,*js-indent*))
              (*js-indent-level* ,*js-indent-level*)))
          `(transform-quasi-quoted-js-to-quasi-quoted-string
            ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
              node (lambda (node)
                     (transform-quasi-quoted-js-to-quasi-quoted-string node))))))
     spliced?)))

(def method transform ((to (eql 'quasi-quoted-string)) (input js-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-js-to-quasi-quoted-string input args))
