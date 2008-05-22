;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(define-syntax (quasi-quoted-xml :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-xml-quasi-quote (mapcar 'body-of result))
                                       (first result))))
    (&key (start-character #\<)
          (end-character #\>)
          (unquote-character #\,)
          (splice-character #\@)
          (transformation-pipeline nil)
          (dispatched-quasi-quote-name "xml"))
  (bind ((original-reader-on-start-character   (multiple-value-list (get-macro-character* start-character *readtable*)))
         (original-reader-on-end-character     (when end-character
                                                 (multiple-value-list (get-macro-character* end-character *readtable*))))
         (original-reader-on-unquote-character (multiple-value-list (get-macro-character* unquote-character *readtable*))))
    (awhen (first original-reader-on-start-character)
      (simple-style-warning "Installing the XML reader on character ~S while it already has a reader installed: ~S" start-character it))
    (set-quasi-quote-syntax-in-readtable
     (lambda (body dispatched?)
       (when (< (length body) 1)
         (simple-reader-error nil "Syntax error in XML syntax: no element name is given?"))
       `(xml-quasi-quote ,(= 1 *quasi-quote-depth*) ,dispatched? ,body ,transformation-pipeline))
     (lambda (body spliced?)
       ;; that progn is for helping on `<foo ,,@body> not turning into (xml-reader-unquote ,@body nil/t).
       ;; see test test/xml/nested-through-macro-using-lisp-quasi-quote2 for a reproduction of it.
       `(xml-reader-unquote (progn ,body) ,spliced?))
     :nested-quasi-quote-wrapper (lambda (body dispatched?)
                                   (when (< (length body) 1)
                                     (simple-reader-error nil "Syntax error in XML syntax: no element name is given?"))
                                   (if dispatched?
                                       body
                                       `(xml-quasi-quote/nested ,body)))
     :start-character start-character
     :end-character end-character
     :unquote-character unquote-character
     :splice-character splice-character
     :readtable-case :preserve
     :dispatched-quasi-quote-name dispatched-quasi-quote-name
     :toplevel-reader-wrapper (lambda (reader)
                                (declare (optimize debug))
                                (named-lambda xml-toplevel-reader-wrapper (stream char)
                                  (block nil
                                    (bind ((next-char (peek-char nil stream nil :eof t)))
                                      (if (and (char= char #\<) ; we are installed on the less-then sign...
                                               (or (eq next-char :eof)
                                                   (not (or (alphanumericp next-char)
                                                            (char= unquote-character next-char)))))
                                          (progn
                                            ;; KLUDGE UNREAD-CHAR after a PEEK-CHAR is not allowed by the standard,
                                            ;; but i don't care much: it works fine on lisps with sane stream buffering,
                                            ;; which includes SBCL.
                                            (unread-char start-character stream)
                                            (bind ((*readtable* (copy-readtable)))
                                              ;; disable us and call READ recursively to make things like (< a b) work in unquoted parts
                                              (apply 'set-macro-character start-character original-reader-on-start-character)
                                              (when end-character
                                                (apply 'set-macro-character end-character original-reader-on-end-character))
                                              (apply 'set-macro-character unquote-character original-reader-on-unquote-character)
                                              (return (read stream t 'eof t))))
                                          (funcall reader stream char)))))))))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-XML-TO-~A" name))
                    (&key-position (position '&key args)))
               `(define-syntax ,syntax-name (,@(subseq args 0 (or &key-position (length args)))
                                               &key
                                               (with-inline-emitting #f)
                                               (declarations '())
                                               (indentation-width nil)
                                               (start-character #\<)
                                               (end-character #\>)
                                               (unquote-character #\,)
                                               (splice-character #\@)
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-xml-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                            :start-character start-character
                                                            :end-character end-character
                                                            :unquote-character unquote-character
                                                            :splice-character splice-character)))))
  ;(x xml-emitting-form           '(xml-emitting-form))
  (x string-emitting-form        (list (make-instance 'quasi-quoted-xml-to-quasi-quoted-string
                                                      :text-node-escaping-method text-node-escaping-method
                                                      :indentation-width indentation-width)
                                       (make-instance 'quasi-quoted-string-to-string-emitting-form
                                                      :stream-variable-name stream-variable-name
                                                      :with-inline-emitting with-inline-emitting
                                                      :declarations declarations))
     (stream-variable-name &key (text-node-escaping-method :per-character)))
  (x binary-emitting-form        (list (make-instance 'quasi-quoted-xml-to-quasi-quoted-string
                                                      :text-node-escaping-method text-node-escaping-method
                                                      :indentation-width indentation-width)
                                       (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                                                      :encoding encoding)
                                       (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                                                      :stream-variable-name stream-variable-name
                                                      :with-inline-emitting with-inline-emitting
                                                      :declarations declarations))
     (stream-variable-name &key
                           (text-node-escaping-method :per-character)
                           (encoding *default-character-encoding*))))

;; the xml reader expands into a macro call of this macro. this way the implementation's normal lisp backquote
;; can work fine when mixed with the xml reader. this macro descends into its body as deep as it can, and
;; converts the body to an xml AST, so that the transformations can actually collapse them into constant
;; strings.
(def macro xml-quasi-quote (toplevel? dispatched? form transformation-pipeline &environment env)
  (macrolet ((unless-unquote (value &body forms)
               (once-only (value)
                 `(if (typep ,value 'xml-unquote)
                      ,value
                      (progn
                        ,@forms)))))
    (bind ((expanded-body (recursively-macroexpand-reader-stubs form env))
           (quasi-quote-node (if dispatched?
                                 ;; dispatched `xml(element) xml
                                 (process-dispatched-xml-reader-body expanded-body transformation-pipeline)
                                 ;; normal <>-based xml
                                 (process-<>-xml-reader-body expanded-body transformation-pipeline))))
      (if toplevel?
          (run-transformation-pipeline quasi-quote-node)
          quasi-quote-node))))

(def macro unless-unquote (value &body forms)
  (once-only (value)
    `(if (typep ,value 'xml-unquote)
         ,value
         (progn
           ,@forms))))

(def function process-dispatched-xml-reader-body (form transformation-pipeline)
  (labels
      ((expand (form)
         (typecase form
           (cons
            (case (first form)
              (xml-reader-unquote
               (assert (= (length form) 3))
               (make-xml-unquote (second form) (third form)))
              ((or xml-quasi-quote xml-quasi-quote/nested) (error "How did this happen? Send a unit test, please!"))
              (t form)))
           (t form)))
       (recurse (form)
         (typecase form
           (string (make-xml-text form))
           (null (error "Null as an xml element?! For a better error message send a unit test or a patch, please!"))
           (symbol (make-xml-element (name-as-string form)))
           (cons
            (setf form (expand form)) ;; TODO ?
            (unless-unquote form
              (bind ((name (aif (pop form)
                                (expand it)
                                (simple-reader-error nil "No xml element name?")))
                     (attributes (expand (pop form)))
                     (children form))
                (assert (or (listp attributes) (typep attributes 'syntax-node)))
                (make-xml-element
                    (unless-unquote name
                      (name-as-string name))
                    (unless-unquote attributes
                      (iter (for (name value) :on attributes :by #'cddr)
                            ;; TODO cleanup attribute syntax, see below
                            (collect (make-xml-attribute
                                      (unless-unquote name (name-as-string name))
                                      (unless-unquote value (princ-to-string value))))))
                  (mapcar #'recurse children)))))
           (t form))))
    (make-xml-quasi-quote transformation-pipeline (recurse form))))

(def function process-<>-xml-reader-body (form transformation-pipeline)
  (labels
      ((recurse (form)
         (typecase form
           (cons
            (case (first form)
              ;; TODO delme (xml-quasi-quote (macroexpand form env))
              (xml-quasi-quote/nested
               (assert (= (length form) 2))
               (bind ((form (second form)))
                 (etypecase form
                   (syntax-node form)
                   (cons
                    (bind ((name (recurse (pop form)))
                           (attributes (recurse (pop form))))
                      (unless name
                        (simple-reader-error nil "Syntax error in XML syntax, node name is NIL!?"))
                      (when (typep attributes '(or string syntax-node))
                        ;; to make the attribute list of foo optional in <foo <bar>> we only accept
                        ;; unquoted attribute lists in the form of <foo (,@(call-some-lisp)) <bar>>.
                        (push attributes form)
                        (setf attributes nil))
                      (make-xml-element
                          (unless-unquote name (name-as-string name))
                          (unless-unquote attributes (iter (generate element :in attributes)
                                                           (for name = (recurse (next element)))
                                                           ;; TODO this is bullshit here, clean up attribute syntax
                                                           ;; <a (,name ,value) > ,name is interpreted as a full attribute currently
                                                           (if (typep name 'xml-unquote)
                                                               (collect name)
                                                               (bind ((value (recurse (next element))))
                                                                 (collect (make-xml-attribute
                                                                           (unless-unquote name (name-as-string name))
                                                                           (unless-unquote value (princ-to-string value))))))))
                        (mapcar (lambda (el)
                                  (if (stringp el)
                                      (make-xml-text el)
                                      (recurse el)))
                                form))))
                   (null (simple-reader-error nil "Empty xml tag?")))))
              (xml-reader-unquote
               (assert (= (length form) 3))
               (make-xml-unquote (recurse (second form)) (third form)))
              (t
               (iter (for entry :first form :then (cdr entry))
                     (collect (recurse (car entry)) :into result)
                     (cond
                       ((consp (cdr entry))
                        ;; nop, go on looping
                        )
                       ((cdr entry)
                        (setf (cdr (last result)) (recurse (cdr entry)))
                        (return result))
                       (t (return result)))))))
           (syntax-node form)
           (t form))))
    (make-xml-quasi-quote transformation-pipeline (recurse `(xml-quasi-quote/nested ,form)))))

(def function name-as-string (name)
  (etypecase name
    (string name)
    (symbol (symbol-name name))))
