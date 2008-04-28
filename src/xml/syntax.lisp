;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

;; TODO should not construct CLOS instances at read time, but rather read into macro sexps so that the impl's `, works as expected
;; then the macros should recurse down as deep as they can on the well-known macro-names

(define-syntax (quasi-quoted-xml :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-xml-quasi-quote (mapcar 'body-of result))
                                       (first result))))
    (&key (start-character #\<)
          (end-character #\>)
          (unquote-character #\,)
          (splice-character #\@)
          (transformation nil))
  (bind ((original-reader-on-start-character   (multiple-value-list (get-macro-character* start-character *readtable*)))
         (original-reader-on-end-character     (when end-character
                                                 (multiple-value-list (get-macro-character* end-character *readtable*))))
         (original-reader-on-unquote-character (multiple-value-list (get-macro-character* unquote-character *readtable*))))
    (awhen (first original-reader-on-start-character)
      (simple-style-warning "Installing the XML reader on character ~S while it already has a reader installed: ~S" start-character it))
    (set-quasi-quote-syntax-in-readtable
     (lambda (body)
       (when (< (length body) 1)
         (simple-reader-error nil "Syntax error in XML syntax: no element name is given?"))
       `(xml-reader-toplevel-element ,body ,transformation))
     (lambda (body spliced?)
       `(xml-reader-unquote ,body ,spliced?))
     :nested-quasi-quote-wrapper (lambda (body)
                                   (when (< (length body) 1)
                                     (simple-reader-error nil "Syntax error in XML syntax: no element name is given?"))
                                   `(xml-reader-element ,body))
     :start-character start-character
     :end-character end-character
     :unquote-character unquote-character
     :splice-character splice-character
     :readtable-case :preserve
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

(macrolet ((x (name transformation &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-XML-TO-~A" name)))
               `(define-syntax ,syntax-name (,@args &key
                                                    (start-character #\<)
                                                    (end-character #\>)
                                                    (unquote-character #\,)
                                                    (splice-character #\@))
                  (set-quasi-quoted-xml-syntax-in-readtable :transformation ,transformation
                                                            :start-character start-character
                                                            :end-character end-character
                                                            :unquote-character unquote-character
                                                            :splice-character splice-character)))))
  (x xml                         '(xml))
  (x xml-emitting-form           '(xml-emitting-form))
  (x string                      '(quasi-quoted-string string))
  (x string-emitting-form        '(quasi-quoted-string string-emitting-form))
  (x string-stream-emitting-form `(quasi-quoted-string (string-emitting-form :stream-name ,stream-name)) (stream-name))
  (x binary                      '(quasi-quoted-string quasi-quoted-binary binary))
  (x binary-emitting-form        '(quasi-quoted-string quasi-quoted-binary binary-emitting-form))
  (x binary-stream-emitting-form `(quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name)) (stream-name)))

;; the xml reader expands into a macro call of this macro. this way the implementation's normal lisp backquote
;; can work fine when mixed with the xml reader. this macro descends into its body as deep as it can, and
;; converts the body to an xml AST, so that the transformations can actually collapse them into constant
;; strings.
(def macro xml-reader-toplevel-element (form transformation)
  (labels ((recurse (form)
             (typecase form
               (cons
                (case (first form)
                  (xml-reader-toplevel-element (assert nil () "How on earth did this happen?!"))
                  (xml-reader-element
                   (bind ((form (second form)))
                     (etypecase form
                       (syntax-node form)
                       (cons
                        (bind ((name (recurse (pop form)))
                               (attributes (recurse (pop form))))
                          (unless name
                            (simple-reader-error nil "Syntax error in XML syntax, node name is NIL!?"))
                          (macrolet ((unless-unquote (value &body forms)
                                       (once-only (value)
                                         `(if (typep ,value 'xml-unquote)
                                              ,value
                                              (progn
                                                ,@forms)))))
                            (when (typep attributes 'syntax-node)
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
                                      form)))))
                       (null (simple-reader-error nil "Empty xml tag?")))))
                  (xml-reader-unquote (make-xml-unquote (second form) (third form)))
                  (t form)))
               (syntax-node form)
               (t form))))
    (chain-transform transformation (make-xml-quasi-quote (recurse `(xml-reader-element ,form))))))

(progn
  ;; these two macros are never actually expanded. they are used as markers for the
  ;; xml-reader-toplevel-element macro to convert their bodies into xml syntax node
  ;; while descending into its body.
  (def macro xml-reader-element (body)
    (declare (ignore body))
    (error "this macro is just a marker and it shouldn't be reached while macroexpanding"))

  (def macro xml-reader-unquote (body spliced?)
    (declare (ignore body spliced?))
    (error "this macro is just a marker and it shouldn't be reached while macroexpanding")))

(def function name-as-string (name)
  (etypecase name
    (string name)
    (symbol (symbol-name name))))
