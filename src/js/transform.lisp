;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def (condition* e) js-compile-error (error)
  ((walked-form nil)))

(def condition* simple-js-compile-error (js-compile-error simple-error)
  ())

(def function simple-js-compile-error (walked-form message &rest args)
  (error 'simple-js-compile-error :walked-form walked-form :format-control message :format-arguments args))

(def special-variable *js-indent-level* 0)

(def special-variable *in-js-statement-context* #t)

(def function make-indent ()
  (awhen (indentation-width-of *transformation*)
    (list (make-string-of-spaces (* it *js-indent-level*)))))

(def with-macro with-increased-indent* (really?)
  (if really?
      (bind ((*js-indent-level* (1+ *js-indent-level*)))
        (-body-))
      (-body-)))

(def with-macro with-increased-indent ()
  (with-increased-indent* #t
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

(def (function oie) to-js-literal (value)
  (etypecase value
    (string (concatenate 'string "'" (escape-as-js-string value) "'"))
    (integer (princ-to-string value))
    (float (format nil "~F" value))
    (ratio (concatenate 'string "(" (princ-to-string (numerator value)) " / " (princ-to-string (denominator value)) ")"))
    (character (concatenate 'string "'" (escape-as-js-string (string value)) "'"))
    (symbol (bind ((js-value (gethash value *js-literals*)))
              (assert js-value () "~S is not a valid js literal" value)
              js-value))))

(def (function ioe) to-js-boolean (value)
  (if value "true" "false"))

(def definer transform-function (name args &body body)
  `(def function ,name ,args
     (with-lexical-transform-functions
       ,@body)))

(def transform-function transform-incf-like (node plus-plus plus-equal)
  (bind ((arguments (arguments-of node)))
    (ecase (length arguments)
      (1 `(,plus-plus ,(recurse (first arguments))))
      (2 `(,(recurse (first arguments)) " " ,plus-equal " " ,(recurse (second arguments)))))))

(def transform-function transform-vector-like (node)
  `(#\[
    ,@(iter (for argument :in (arguments-of node))
            (unless (first-time-p)
              (collect ", "))
            (collect (recurse argument)))
    #\]))

(def transform-function transform-map-like (node &key (destructively-into :inplace))
  (bind ((arguments (arguments-of node))
         (fn (pop arguments))
         (fn-processed (cond
                         ((typep fn 'walked-lexical-variable-reference-form)
                          (lisp-name-to-js-name (name-of fn)))
                         ((and (typep fn 'constant-form)
                               (symbolp (value-of fn))
                               (not (js-literal-name? (value-of fn))))
                          (lisp-name-to-js-name (value-of fn)))
                         (t (recurse fn))))
         (sequence (pop arguments))
         (idx-var (unique-js-name "_idx"))
         (array-var (unique-js-name "_src"))
         (result nil))
    (when arguments
      (simple-js-compile-error node "TODO: js compiler doesn't support iterating multiple sequences using map constructs yet"))
    (when (eq :inplace destructively-into)
      (setf destructively-into array-var))
    (when (and (not *in-js-statement-context*)
               (not destructively-into))
      (setf destructively-into (unique-js-name "_tgt"))
      (setf result (list (format "var ~A = [];~%" destructively-into))))
    (bind ((result `(,(format nil "var ~A = " array-var)
                     ,(recurse sequence)
                     ,(format nil ";~%for (~A = 0; ~A < ~A.length; ~A++) {~%" idx-var idx-var array-var idx-var)
                     ,@(when destructively-into
                         `(,array-var "[" ,idx-var "] = "))
                     "("
                     ,fn-processed
                     ,(format nil ")(~A[~A])~%}" array-var idx-var))))
      (if *in-js-statement-context*
          result
          `("(function () { "
            ,result
            ,(format nil "; return ~A; })()" destructively-into))))))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name . body) :in entries)
                        (collect `(def js-special-form ,name
                                    ,@body))))))
  (frob
   (|incf|   (transform-incf-like -node- "++" "+="))
   (|decf|   (transform-incf-like -node- "--" "-="))
   (|vector| (transform-vector-like -node-))
   (|list|   (transform-vector-like -node-))
   (|map|    (transform-map-like -node-))
   (|not|    (unless (length= 1 (arguments-of -node-))
               (simple-js-compile-error -node- "The not operator expects exactly one argument!"))
             `("!(" ,(recurse (first (arguments-of -node-))) ")"))
   (|aref|   (bind ((arguments (arguments-of -node-)))
               (unless (rest arguments)
                 (simple-js-compile-error -node- "The aref operator needs at least two arguments!"))
               `(,(recurse (first arguments))
                  ,@(iter (for argument :in (rest arguments))
                          (collect `(#\[
                                     ,(recurse argument)
                                     #\]))))))
   (|elt|    (bind ((arguments (arguments-of -node-)))
               (unless (length= 2 arguments)
                 (simple-js-compile-error -node- "An elt operator with ~A arguments?" (length arguments)))
               `(,(recurse (first arguments))
                  #\[
                  ,(recurse (second arguments))
                  #\])))
   (|array|  (bind ((arguments (arguments-of -node-)))
               `(#\[
                 ,@(recurse-as-comma-separated arguments
                                               (lambda (node)
                                                 (if (and (typep node 'js-unquote)
                                                          (spliced-p node))
                                                     (make-string-unquote
                                                      `(transform-quasi-quoted-js-to-quasi-quoted-string/array-elements
                                                        ,(form-of node)))
                                                     (recurse node))))
                 #\])))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/array-elements (elements)
  (iter (for element :in-sequence elements)
        (unless (first-iteration-p)
          (collect ", "))
        (collect (transform-quasi-quoted-js-to-quasi-quoted-string element))))

(def special-variable *js-block-nesting-level* 0)

(def (function i) in-toplevel-js-block? ()
  (<= *js-block-nesting-level* 1))

(def with-macro within-nested-js-block* (really?)
  (if really?
      (bind ((*js-block-nesting-level* (1+ *js-block-nesting-level*)))
        (-body-))
      (-body-)))

(def with-macro within-nested-js-block ()
  (within-nested-js-block* #t
    (-body-)))

(def transform-function transform-progn (node &key (wrap? nil wrap-provided?) (nest? #t) (increase-indent? #f))
  (within-nested-js-block* nest?
    (bind ((body (cl-walker:body-of node))
           (*in-js-statement-context* #t))
      (unless wrap-provided?
        (setf wrap? (and (rest body)
                         (not (in-toplevel-js-block?)))))
      `(,@(when wrap? '(#\{))
        ,@(with-increased-indent* increase-indent?
            (iter (for statement :in body)
                  (collect #\Newline)
                  (awhen (make-indent)
                    (collect it))
                  (collect (recurse statement))
                  (collect #\;)))
        ,@(when wrap? `(#\Newline ,@(make-indent) #\}))))))

(def transform-function transform-implicit-progn (node)
  (if (typep node 'implicit-progn-mixin)
      (transform-progn node :wrap? #t)
      `(,(recurse node) #\; #\Newline)))

(def generic transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument (node)
  (:method ((node required-function-argument-form))
    (lisp-name-to-js-name (name-of node))))

(macrolet ((frob (&rest entries)
             `(with-lexical-transform-functions
                (defgeneric transform-quasi-quoted-js-to-quasi-quoted-string* (form)
                  ,@(iter (for (type . body) :in entries)
                          (collect `(:method ((-node- ,type))
                                      ,@body)))))))
  (frob
   (variable-reference-form
    (lisp-name-to-js-name (name-of -node-)))
   (progn-form
    (transform-progn -node-))
   (if-form
    (bind ((condition (condition-of -node-))
           (then (then-of -node-))
           (else (else-of -node-)))
      (if (or *in-js-statement-context*
              (typep then 'progn-form)
              (typep else 'progn-form))
          `("if(" ,(recurse condition) ")" #\Newline
                  ,@(transform-implicit-progn then)
                  ,@(when else `(#\Newline "else " ,@(transform-implicit-progn else))))
          `("(" ,(recurse condition) ") ? ("
                ,(recurse then)
                ") : ("
                ,(if else
                     (recurse else)
                     "undefined")
                ")"))))
   (lambda-application-form
    (bind ((operator (operator-of -node-))
           (arguments (arguments-of -node-)))
      (assert (typep operator 'lambda-function-form))
      `(,(recurse operator)
         #\(
         ,@(recurse-as-comma-separated arguments)
         #\) )))
   (lambda-function-form
    `("function ("
      ,@(recurse-as-comma-separated (arguments-of -node-) 'transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument)
      ") {"
      ,@(transform-progn -node- :wrap? #f)
      #\Newline
      "}"))
   (application-form
    (bind ((operator (operator-of -node-))
           (arguments (arguments-of -node-))
           (operator-name (lisp-name-to-js-name (lisp-operator-name-to-js-operator-name operator))))
      (cond
        ((js-special-form? operator)
         (bind ((handler (gethash operator *js-special-forms*)))
           (funcall handler -node-)))
        ((js-operator-name? operator)
         `("("
           ,@(iter (for el :in arguments)
                   (unless (first-time-p)
                     (collect " ")
                     (collect operator-name)
                     (collect " "))
                   (collect (recurse el)))
           ")"))
        (t
         (bind ((dotted? (starts-with #\. operator-name)))
           (if dotted?
               `(,(recurse (first arguments))
                  ,operator-name #\(
                  ,@(recurse-as-comma-separated (rest arguments))
                  #\) )
               `(,operator-name #\(
                                ,@(recurse-as-comma-separated arguments)
                                #\) )))))))
   (constant-form
    (to-js-literal (value-of -node-)))
   (variable-binding-form
    (bind ((indent (make-indent)))
      (within-nested-js-block
        (with-increased-indent
          `(,@(unless (in-toplevel-js-block?) (list "{"))
            ,@(iter (for (name . value) :in (bindings-of -node-))
                    (collect `(#\Newline ,@(make-indent) ,(lisp-name-to-js-name name) " = " ,(recurse value) ";")))
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
                  ,@(transform-progn -node- :wrap? #f :increase-indent? #t)
                  #\Newline
                  "}"))
   (return-from-form
    `("return" ,@(awhen (result-of -node-)
                        (list #\space (recurse it)))))
   (instantiate-form
    `("new " ,(lisp-name-to-js-name (type-to-instantiate-of -node-))
             "("
             ,@(recurse-as-comma-separated (arguments-of -node-))
             ")"))
   (create-form
    (bind ((elements (elements-of -node-)))
      `("{ "
        ,@(with-increased-indent
           (iter (with indent = `(#\, #\Newline ,@(make-indent)))
                 (for (name . value) :in elements)
                 (unless (first-time-p)
                   (collect indent))
                 (collect (typecase name
                            (string name)
                            (keyword (lisp-name-to-js-name name))
                            (integer (princ-to-string name))
                            (t (simple-js-compile-error "Don't know how to deal with ~S as a name in create form ~S"
                                                        name (source-of -node-)))))
                 (collect ": ")
                 (collect (recurse value))))
        "}")))
   (slot-value-form
    (bind ((object (object-of -node-))
           (slot-name (slot-name-of -node-)))
      (if (symbolp slot-name)
          `(,(recurse object)
             #\.
             ,(lisp-name-to-js-name slot-name))
          `(,(recurse object)
             #\[
             ,(recurse slot-name)
             #\]))))))

(def (transformation e) quasi-quoted-js-to-quasi-quoted-string ()
  ((indentation-width nil)
   (output-prefix nil)
   (output-postfix nil))
  'transform-quasi-quoted-js-to-quasi-quoted-string)

(def function transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'js-quasi-quote fn))

(def function transform-quasi-quoted-js-to-quasi-quoted-string (node)
  (transformation-typecase node
    ((or number string character) (to-js-literal node))
    (walked-form    (transform-quasi-quoted-js-to-quasi-quoted-string* node))
    (js-unquote     (transform-quasi-quoted-js-to-quasi-quoted-string/unquote node))
    (js-quasi-quote (if (compatible-transformation-pipelines? *transformation-pipeline*
                                                              (transformation-pipeline-of node))
                        (make-string-quasi-quote (rest (transformation-pipeline-of node))
                                                 `(,(output-prefix-of *transformation*)
                                                   ,(transform-quasi-quoted-js-to-quasi-quoted-string (body-of node))
                                                   ,(output-postfix-of *transformation*)))
                        (transform node)))
    (string-quasi-quote node)))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/unquote (node)
  (assert (typep node 'js-unquote))
  (bind ((spliced? (spliced-p node)))
    (make-string-unquote
     (wrap-runtime-delayed-transformation-form
      (wrap-forms-with-bindings
       (when (indentation-width-of *transformation*)
         `((*js-indent-level* (+ *js-indent-level* ,*js-indent-level*))))
       (if spliced?
           `(mapcar 'transform-quasi-quoted-js-to-quasi-quoted-string
                    ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
                      node 'transform-quasi-quoted-js-to-quasi-quoted-string))
           `(transform-quasi-quoted-js-to-quasi-quoted-string
             ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
               node 'transform-quasi-quoted-js-to-quasi-quoted-string)))))
     spliced?)))
