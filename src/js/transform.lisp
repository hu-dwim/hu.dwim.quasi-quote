;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def special-variable *js-indent-level* 0)
(def special-variable *in-js-statement-context* #t)
(def special-variable *js-operator-precedence*)

(def function make-indent ()
  (awhen (indentation-width-of *transformation*)
    (list (make-string-of-spaces (* it *js-indent-level*)))))

(def function make-newline-and-indent ()
  (list* #\Newline (make-indent)))

(def with-macro with-increased-indent* (really?)
  (if really?
      (bind ((*js-indent-level* (1+ *js-indent-level*)))
        (-body-))
      (-body-)))

(def with-macro with-increased-indent ()
  (with-increased-indent* #t
    (-body-)))

(def function wrap-runtime-delayed-js-transformation-form (form)
  (wrap-runtime-delayed-transformation-form
   (wrap-forms-with-bindings
    (when (indentation-width-of *transformation*)
      `((*js-indent-level* (+ *js-indent-level* ,*js-indent-level*))))
    form)))

(def (function oe) hyphened-to-camel-case (input)
  (declare (type string input))
  (bind ((pieces (cl-ppcre:split "-" input)))
    (if (rest pieces)
        (bind ((*print-pretty* #f))
          (with-output-to-string (str)
            (iter (for piece :in pieces)
                  (write-string (if (first-time-p)
                                    piece
                                    (capitalize-first-letter! piece))
                                str)
                  (collect piece))))
        input)))

(def (function oe) lisp-name-to-js-name (symbol &key operator)
  (etypecase symbol
    (js-unquote
     (make-string-unquote (wrap-runtime-delayed-js-transformation-form
                           `(lisp-name-to-js-name ,(form-of symbol)))))
    (symbol
     (if (and (not operator)
              (gethash (string-downcase symbol) *js-reserved-keywords*))
         (bind ((name (symbol-name symbol)))
           (concatenate 'string name (princ-to-string (mod (sxhash name) 10000))))
         (hyphened-to-camel-case (symbol-name symbol))))))

(def function to-js-operator-name (name)
  (lisp-name-to-js-name (lisp-operator-name-to-js-operator-name name) :operator #t))

(def macro with-root-operator-precedence (&body body)
  `(bind ((*js-operator-precedence* most-positive-fixnum))
     ,@body))

(def macro with-operator-precedence (operator &body body)
  `(bind ((*js-operator-precedence* ,(if (or (numberp operator)
                                             (consp operator))
                                         (if (eq 'quote (first operator))
                                             (aprog1
                                                 (operator-precedence (lisp-operator-name-to-js-operator-name (second operator)))
                                               (assert it () "~S is not a valid JS operator name" operator))
                                             operator)
                                         `(aprog1
                                              (operator-precedence (lisp-operator-name-to-js-operator-name ,operator))
                                            (assert it () "~S is not a valid JS operator name" ,operator)))))
     ,@body))

(def macro with-wrapping-based-on-operator-precedence (operator &body body)
  (with-unique-names (needs-parens? result parent-operator-precedence)
    `(bind ((,parent-operator-precedence *js-operator-precedence*))
       (with-operator-precedence ,operator
         (bind ((,needs-parens? (> *js-operator-precedence* ,parent-operator-precedence))
                (,result (progn
                           ;; (format *debug-io* "wrapping at operator: ~S, precedence ~A -> ~A~%" ',operator ,parent-operator-precedence *js-operator-precedence*)
                           ,@body)))
           (if ,needs-parens?
               `("(" ,@,result ")")
               ,result))))))

(def (function oe) to-js-literal (value)
  (etypecase value
    (string (concatenate 'string "'" (escape-as-js-string value) "'"))
    (integer (princ-to-string value))
    (float (format nil "~F" value))
    (ratio (concatenate 'string "(" (princ-to-string (numerator value)) " / " (princ-to-string (denominator value)) ")"))
    (character (concatenate 'string "'" (escape-as-js-string (string value)) "'"))
    (symbol (bind ((js-value (gethash value *js-literals*)))
              (assert js-value () "~S is not a valid js literal" value)
              js-value))
    (vector `("["
              ,@(iter (for element :in-vector value)
                      (unless (first-time-p)
                        (collect ", "))
                      (collect (to-js-literal element)))
              "]"))
    (js-unquote (make-string-unquote
                 (wrap-runtime-delayed-js-transformation-form
                  `(to-js-literal ,(form-of value)))))))

(def (function ioe) to-js-boolean (value)
  (if value "true" "false"))

(def definer transform-function (name args &body body)
  `(def function ,name ,args
     (with-lexical-transform-functions
       ,@body)))

(def transform-function transform-incf-like (node plus-plus plus-equal)
  (bind ((arguments (arguments-of node)))
    (ecase (length arguments)
      (1 (with-wrapping-based-on-operator-precedence '=
           `(,(recurse (first arguments)) " = " ,plus-plus ,(recurse (first arguments)))))
      (2 (with-wrapping-based-on-operator-precedence '+=
           `(,(recurse (first arguments)) " " ,plus-equal " " ,(recurse (second arguments))))))))

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
      (js-compile-error node "TODO: js compiler doesn't support iterating multiple sequences using map constructs yet"))
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
                ,@(iter (for (name-spec . body) :in entries)
                        (dolist (name (ensure-list name-spec))
                          (collect `(def js-special-form ,name
                                        ,@body)))))))
  (frob
   (|null|   (bind ((arguments (arguments-of -node-)))
               (assert (length= 1 arguments))
               (with-wrapping-based-on-operator-precedence '==
                 `(,(recurse (first arguments)) " == null"))))
   (|incf|   (transform-incf-like -node- "++" "+="))
   (|decf|   (transform-incf-like -node- "--" "-="))
   (|vector| (transform-vector-like -node-))
   (|list|   (transform-vector-like -node-))
   (|map|    (transform-map-like -node-))
   (|with|   (bind ((arguments (arguments-of -node-)))
               `("with (" ,(recurse (first arguments)) ") " ,(transform-statements (rest arguments) :wrap? #t))))
   ;; KLUDGE need to handle 'not' specially, because the one at application-form can only handle infix operators for now
   (|not|    (unless (length= 1 (arguments-of -node-))
               (js-compile-error -node- "The 'not' operator expects exactly one argument!"))
             (with-wrapping-based-on-operator-precedence 'not
               `("!" ,(recurse (first (arguments-of -node-))))))
   (|aref|   (bind ((arguments (arguments-of -node-)))
               (unless (rest arguments)
                 (js-compile-error -node- "The 'aref' operator needs at least two arguments!"))
               (with-wrapping-based-on-operator-precedence 'member
                 `(,(recurse (first arguments))
                   ,@(iter (for argument :in (rest arguments))
                           (collect `(#\[
                                      ,(recurse argument)
                                      #\])))))))
   (|elt|    (bind ((arguments (arguments-of -node-)))
               (unless (length= 2 arguments)
                 (js-compile-error -node- "An elt operator with ~A arguments?" (length arguments)))
               (with-wrapping-based-on-operator-precedence 'member
                 `(,(recurse (first arguments))
                   #\[
                   ,(recurse (second arguments))
                   #\]))))
   ((1+ 1-)  (bind ((arguments (arguments-of -node-))
                    (argument (first arguments))
                    (operator (operator-of -node-)))
               (unless (length= 1 arguments)
                 (js-compile-error -node- "More than one argument to ~S?" operator))
               (if (typep argument 'variable-reference-form)
                   (with-wrapping-based-on-operator-precedence '++
                     (ecase operator
                       (1+ `("++" ,(recurse argument)))
                       (1- `("--" ,(recurse argument)))))
                   (with-wrapping-based-on-operator-precedence '+
                     (ecase operator
                       (1+ `("1 + " ,(recurse argument)))
                       (1- `("1 + " ,(recurse argument))))))))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/array-elements (elements)
  (iter (for element :in-sequence elements)
        (unless (first-iteration-p)
          (collect ", "))
        (collect (transform-quasi-quoted-js-to-quasi-quoted-string element))))

(def (function i) in-toplevel-js-block? ()
  ;; TODO ?
  #f)

(def transform-function variable-binding-form-statement-prefix-generator (node)
  (iter (for (name . value) :in (bindings-of node))
        (when name
          (collect (with-root-operator-precedence
                     `(#\Newline ,@(make-indent) "var " ,(lisp-name-to-js-name name) " = " ,(recurse value) ";"))))))

(def transform-function transform-statements (thing &key (wrap? nil wrap-provided?))
  (with-root-operator-precedence
    (bind ((node (labels ((drop-progns (node)
                            (typecase node
                              (progn-form (bind ((statements (cl-walker:body-of node)))
                                            (if (and (length= 1 statements)
                                                     (typep (first statements) 'implicit-progn-mixin)) ; don't strip the last progn
                                                (drop-progns (first statements))
                                                node)))
                              (t node))))
                   ;; skip the progn when it's not needed to avoid double {} wrapping
                   (drop-progns thing)))
           (*in-js-statement-context* #t)
           (statement-prefix-generator (constantly nil))
           (statements (etypecase node
                         (variable-binding-form
                          (setf statement-prefix-generator (lambda ()
                                                             (variable-binding-form-statement-prefix-generator node)))
                          (setf wrap? (not (in-toplevel-js-block?)))
                          (setf wrap-provided? #t)
                          (cl-walker:body-of node))
                         (implicit-progn-mixin
                          (cl-walker:body-of node))
                         (list
                          node))))
      (unless wrap-provided?
        (setf wrap? (and (rest statements)
                         (not (in-toplevel-js-block?)))))
      (append
       (when wrap?
         `(#\Newline ,@(make-indent) "{"))
       (with-increased-indent
         (append
          (funcall statement-prefix-generator)
          (iter (for statement :in statements)
                (collect #\Newline)
                (awhen (make-indent)
                  (collect it))
                ;; don't use RECURSE, because it rebinds *in-js-statement-context* to #f
                (collect (transform-quasi-quoted-js-to-quasi-quoted-string statement))
                (when (requres-semicolon-postfix? statement)
                  (collect #\;)))))
       (when wrap?
         `(#\Newline ,@(make-indent) "}"))))))

(def function requres-semicolon-postfix? (statement)
  (not (typep statement '(or if-form try-form))))

(def generic transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument (node)
  (:method ((node required-function-argument-form))
    (lisp-name-to-js-name (name-of node))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name-value-pairs (input-elements)
  (iter (with indent = `(#\, #\Newline ,@(make-indent)))
        (with elements = input-elements)

        (for name = (pop elements))
        (while name)
        (unless (first-time-p)
          (collect indent))
        (collect (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name name))
        (if (and (typep name 'js-unquote)
                 (spliced? name))
            (when elements
              (js-compile-error nil "Unexpected element(s) after a spliced unquote in a create form: ~S" elements))
            (collect ": "))

        (for value = (pop elements))
        (while value)
        (collect (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value value))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name (name)
  (typecase name
    (string        name)
    (symbol        (lisp-name-to-js-name name))
    (constant-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name (value-of name)))
    (variable-reference-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name (name-of name)))
    (js-unquote    (make-string-unquote
                    (wrap-runtime-delayed-js-transformation-form
                     (if (spliced? name)
                         `(transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name-value-pairs ,(form-of name))
                         `(transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name             ,(form-of name))))))
    (t (transform-quasi-quoted-js-to-quasi-quoted-string name))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value (value)
  (typecase value
    (symbol        (lisp-name-to-js-name value))
    (constant-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value (value-of value)))
    (variable-reference-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value (name-of value)))
    (js-unquote    (if (spliced? value)
                       (js-compile-error nil "Spliced unquoting is not supported at value position in create forms")
                       (make-string-unquote
                        (wrap-runtime-delayed-js-transformation-form
                         `(transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value ,(form-of value))))))
    (t (transform-quasi-quoted-js-to-quasi-quoted-string value))))

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
    (bind ((statements (cl-walker:body-of -node-)))
      (if (and (length= 1 statements)
               (typep (first statements) '(or variable-binding-form function-binding-form)))
          ;; skip the progn when it's not needed to avoid double {} wrapping
          (recurse (first statements))
          (transform-statements -node-))))
   (if-form
    (bind ((condition (condition-of -node-))
           (then (then-of -node-))
           (else (else-of -node-)))
      (when (and (typep else 'constant-form)
                 (eq nil (value-of else)))
        (setf else nil))
      (if *in-js-statement-context*
          (flet ((transform-if-block (node)
                   (typecase node
                     (implicit-progn-mixin
                      (transform-statements node))
                     ((or if-form try-form for-form)
                      (with-increased-indent
                        `(#\Newline
                          ,@(make-indent)
                          ;; don't use RECURSE here, because it rebinds *in-js-statement-context* to #f
                          ,(transform-quasi-quoted-js-to-quasi-quoted-string node))))
                     (t
                      (with-increased-indent
                        `(#\Newline
                          ,@(make-indent)
                          ;; don't use RECURSE here, because it rebinds *in-js-statement-context* to #f
                          ,(transform-quasi-quoted-js-to-quasi-quoted-string node)
                          ,(when (requres-semicolon-postfix? node)
                             #\;)))))))
            `("if (" ,(recurse condition) ")"
                     ,@(transform-if-block then)
                     ,@(if else
                           `(#\Newline ,@(make-indent) "else"
                                       ,@(transform-if-block else)))))
          (with-wrapping-based-on-operator-precedence 'conditional
            (when (or (typep then 'implicit-progn-mixin)
                      (typep else 'implicit-progn-mixin))
              (js-compile-error -node- "if's may not have multiple statements in their then/else branch when they are used in expression context"))
            `(,(recurse condition) " ? "
                  ,(recurse then)
               " : "
               ,(if else
                    (recurse else)
                    "undefined"))))))
   (lambda-application-form
    (bind ((operator (operator-of -node-))
           (arguments (arguments-of -node-)))
      (assert (typep operator 'lambda-function-form))
      `("(" ,(recurse operator) ")"
        "(" ,@(with-operator-precedence 'comma
                (recurse-as-comma-separated arguments))
        ")" )))
   (lambda-function-form
    `("function ("
      ,@(with-operator-precedence 'comma
          (recurse-as-comma-separated (arguments-of -node-) 'transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument))
      ")"
      ,@(transform-statements -node- :wrap? #t)))
   (application-form
    (bind ((arguments (arguments-of -node-))
           (operator (operator-of -node-)))
      (if (js-special-form? operator)
          (bind ((handler (gethash operator *js-special-forms*)))
            (funcall handler -node-))
          (with-wrapping-based-on-operator-precedence (or (operator-precedence (lisp-operator-name-to-js-operator-name operator))
                                                          #.(operator-precedence 'function-call))
            ;; (format *debug-io* "application-form of ~S~%" operator)
            (bind ((js-operator-name (to-js-operator-name operator)))
              (if (js-operator-name? operator)
                  ;; TODO it can only handle infix operators. due to this |not| needs its own special-form handler
                  (iter (for el :in arguments)
                        (unless (first-time-p)
                          (collect " ")
                          (collect js-operator-name)
                          (collect " "))
                        (collect (recurse el)))
                  (bind ((dotted? (starts-with #\. js-operator-name)))
                    (if dotted?
                        (with-operator-precedence 'member
                          `(,(recurse (first arguments))
                             ,js-operator-name #\(
                             ,@(recurse-as-comma-separated (rest arguments))
                             #\) ))
                        (with-operator-precedence 'comma
                          `(,js-operator-name #\(
                                              ,@(recurse-as-comma-separated arguments)
                                              #\) ))))))))))
   (constant-form
    (to-js-literal (value-of -node-)))
   (macrolet-form
    (transform-statements -node-))
   (variable-binding-form
    (transform-statements -node-))
   (setq-form
    (with-wrapping-based-on-operator-precedence '=
      `(,(recurse (variable-of -node-)) " = " ,(recurse (value-of -node-)))))
   (function-definition-form
    `("function " ,(lisp-name-to-js-name (name-of -node-))
                  "("
                  ,@(iter (for argument :in (arguments-of -node-))
                          (collect (transform-quasi-quoted-js-to-quasi-quoted-string/lambda-argument argument)))
                  ")"
                  ,@(transform-statements -node- :wrap? #t)))
   (flet-form
    (flet ((collect-js-names-of-variable-references (node)
             (mapcar (compose 'lisp-name-to-js-name 'name-of)
                     (collect-variable-references node))))
      (iter (with variable-references = (collect-js-names-of-variable-references (cl-walker:body-of -node-)))
            (for (lisp-name . body) :in (bindings-of -node-))
            (for name = (lisp-name-to-js-name lisp-name))
            (when (some (lambda (reference)
                          (string= name reference))
                        variable-references)
              (js-compile-warning "Found a variable reference to a name that names an flet definition (~S). In the JavaScript output flet definitions are in the same namespace as the variables!" name))
            (appendf variable-references (collect-js-names-of-variable-references body))
            (collect `(,@(make-newline-and-indent) "var " ,name " = " ,(recurse body) ";") :into result)
            (finally (return (cons result (transform-statements (cl-walker:body-of -node-) :wrap? #f)))))))
   (return-from-form
    `("return" ,@(awhen (result-of -node-)
                        (list #\space (recurse it)))))
   (instantiate-form
    (with-wrapping-based-on-operator-precedence 'new
      `("new " ,(lisp-name-to-js-name (type-to-instantiate-of -node-))
               "("
               ,@(recurse-as-comma-separated (arguments-of -node-))
               ")")))
   (create-form
    `("{ "
      ,@(with-increased-indent
         (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name-value-pairs (elements-of -node-)))
      "}"))
   (array-form
    (bind ((elements (elements-of -node-)))
      `(#\[
        ,@(recurse-as-comma-separated elements
                                      (lambda (node)
                                        (if (and (typep node 'js-unquote)
                                                 (spliced? node))
                                            (make-string-unquote
                                             (wrap-runtime-delayed-js-transformation-form
                                              `(transform-quasi-quoted-js-to-quasi-quoted-string/array-elements
                                                ,(form-of node))))
                                            (recurse node))))
        #\])))
   (regexp-form
    `("/"
      ,(regexp-of -node-)
      "/"))
   (for-form
    `("for ("
      ,@(recurse-as-comma-separated (variables-of -node-))
      "; "
      ,(recurse (looping-condition-of -node-))
      "; "
      ,@(recurse-as-comma-separated (steps-of -node-))
      ")"
      ,@(transform-statements (body-of -node-) :wrap? #t)))
   (slot-value-form
    (bind ((object (object-of -node-))
           (slot-name (slot-name-of -node-)))
      (with-wrapping-based-on-operator-precedence 'member
        (if (symbolp slot-name)
            `(,(recurse object)
               #\.
               ,(lisp-name-to-js-name slot-name))
            `(,(recurse object)
               #\[
               ,(recurse slot-name)
               #\])))))
   (type-of-form
    (bind ((object (object-of -node-)))
      (with-operator-precedence 'typeof
        `("typeof(" ,(recurse object) ")"))))
   (unwind-protect-form
    `(,@(make-newline-and-indent) "try"
      ,@(recurse (protected-form-of -node-))
      ,@(make-newline-and-indent) "finally"
      ,@(transform-statements (cleanup-form-of -node-) :wrap? #t)))
   (try-form
    (bind ((catch-clauses (catch-clauses-of -node-))
           (finally-clause (finally-clause-of -node-)))
      (awhen (find-if [not (typep !1 'catch-form)] catch-clauses)
        (js-compile-error -node- "Expecting only catch caluse here, but got ~A" it))
      (flet ((transform-catch-clause (clause)
               `(,@(make-newline-and-indent) "catch (" ,(lisp-name-to-js-name (variable-name-of clause)) ")"
                 ,@(transform-statements clause :wrap? #t))))
        `(,@(make-newline-and-indent) "try"
          ,@(transform-statements (protected-form-of -node-) :wrap? #t)
          ,@(mapcar #'transform-catch-clause catch-clauses)
          ,@(when finally-clause
              `(,@(make-newline-and-indent) "finally"
                ,@(transform-statements finally-clause :wrap? #t)))))))))

(def (transformation e) quasi-quoted-js-to-quasi-quoted-string ()
  ((indentation-width nil)
   (output-prefix nil)
   (output-postfix nil))
  'transform-quasi-quoted-js-to-quasi-quoted-string/toplevel)

(defmethod print-object ((self quasi-quoted-js-to-quasi-quoted-string) *standard-output*)
  (princ "[JS->String]"))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/toplevel (node)
  (assert (typep node 'js-quasi-quote))
  (with-root-operator-precedence
    (make-string-quasi-quote (rest (transformation-pipeline-of node))
                             `(,(awhen (output-prefix-of *transformation*)
                                       (if (functionp it)
                                           (funcall it)
                                           it))
                                ,(transform-quasi-quoted-js-to-quasi-quoted-string (body-of node))
                                ,(awhen (output-postfix-of *transformation*)
                                        (if (functionp it)
                                            (funcall it)
                                            it))))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'js-quasi-quote fn))

(def function transform-quasi-quoted-js-to-quasi-quoted-string (node)
  (transformation-typecase node
    ((or number string character symbol) (to-js-literal node))
    (walked-form    (transform-quasi-quoted-js-to-quasi-quoted-string* node))
    (js-unquote     (transform-quasi-quoted-js-to-quasi-quoted-string/unquote node))
    (js-quasi-quote (if (compatible-transformation-pipelines? *transformation-pipeline*
                                                              (transformation-pipeline-of node))
                        (transform-quasi-quoted-js-to-quasi-quoted-string/toplevel node)
                        (transform node)))
    (string-quasi-quote node)))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/unquote (node)
  (assert (typep node 'js-unquote))
  (bind ((spliced? (spliced? node)))
    (make-string-unquote
     (wrap-runtime-delayed-js-transformation-form
      (if spliced?
          `(mapcar 'transform-quasi-quoted-js-to-quasi-quoted-string
                   ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
                     node 'transform-quasi-quoted-js-to-quasi-quoted-string))
          `(transform-quasi-quoted-js-to-quasi-quoted-string
            ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
              node 'transform-quasi-quoted-js-to-quasi-quoted-string)))))))
