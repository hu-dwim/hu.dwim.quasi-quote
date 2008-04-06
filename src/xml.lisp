;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(def special-variable *quasi-quoted-xml-nesting-level*)

(define-syntax (quasi-quoted-xml :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-xml-quasi-quote (mapcar 'body-of result))
                                       (first result))))
    (&key (quasi-quote-character #\<)
          (quasi-quote-end-character #\>)
          (unquote-character #\,)
          (splice-character #\@)
          (transform nil))
  (bind ((original-reader-on-quasi-quote-character     (multiple-value-list (get-macro-character quasi-quote-character *readtable*)))
         (original-reader-on-quasi-quote-end-character (when quasi-quote-end-character
                                                         (multiple-value-list (get-macro-character quasi-quote-end-character *readtable*))))
         (original-reader-on-unquote-character         (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-quasi-quote-syntax-in-readtable
     (lambda (body)
       (readtime-chain-transform transform (make-xml-quasi-quote (parse-xml-reader-body nil body))))
     (lambda (body spliced?)
       (make-xml-unquote body spliced?))
     '*quasi-quoted-xml-nesting-level*
     :nested-quasi-quote-wrapper (lambda (body)
                                   (parse-xml-reader-body nil body))
     :quasi-quote-character quasi-quote-character
     :quasi-quote-end-character quasi-quote-end-character
     :unquote-character unquote-character
     :splice-character splice-character
     :toplevel-reader-wrapper (lambda (reader)
                                (lambda (stream char)
                                  (block nil
                                    (bind ((next-char (peek-char nil stream nil :eof t)))
                                      (if (or (eq next-char :eof)
                                              (not (or (alphanumericp next-char)
                                                       (char= unquote-character next-char))))
                                          (progn
                                            ;; KLUDGE UNREAD-CHAR after a PEEK-CHAR is not allowed by the standard,
                                            ;; but i don't care much: it works fine on lisps with sane stream buffering,
                                            ;; which includes SBCL.
                                            (unread-char quasi-quote-character stream)
                                            (bind ((*readtable* (copy-readtable)))
                                              ;; disable us and call READ recursively to make things like (< a b) work in unquoted parts
                                              (apply 'set-macro-character quasi-quote-character original-reader-on-quasi-quote-character)
                                              (when quasi-quote-end-character
                                                (apply 'set-macro-character quasi-quote-end-character original-reader-on-quasi-quote-end-character))
                                              (apply 'set-macro-character unquote-character original-reader-on-unquote-character)
                                              ;;(setf (readtable-case *readtable*) original-readtable-case)
                                              (return (read stream t nil t))))
                                          (funcall reader stream char)))))))))

(define-syntax quasi-quoted-xml-to-xml ()
  (set-quasi-quoted-xml-syntax-in-readtable :transform '(xml)))

(define-syntax quasi-quoted-xml-to-xml-emitting-form ()
  (set-quasi-quoted-xml-syntax-in-readtable :transform '(xml-emitting-form)))

(define-syntax quasi-quoted-xml-to-string ()
  (set-quasi-quoted-xml-syntax-in-readtable :transform '(quasi-quoted-string string)))

(define-syntax quasi-quoted-xml-to-string-emitting-form ()
  (set-quasi-quoted-xml-syntax-in-readtable :transform '(quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-xml-to-binary ()
  (set-quasi-quoted-xml-syntax-in-readtable :transform '(quasi-quoted-string quasi-quoted-binary binary)))

(define-syntax quasi-quoted-xml-to-binary-emitting-form ()
  (set-quasi-quoted-xml-syntax-in-readtable :transform '(quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-xml-to-binary-stream-emitting-form (stream)
  (set-quasi-quoted-xml-syntax-in-readtable :transform `(quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream ,stream))))

(def (function d) parse-xml-reader-body (stream form)
  (etypecase form
    (syntax-node form)
    (cons
     (setf form (mapcar (lambda (el)
                          (if (stringp el)
                              (make-xml-text el)
                              el))
                        form))
     (bind ((name (pop form))
            (attributes (pop form)))
       (unless name
         (simple-reader-error stream "Syntax error in XML syntax, node name is NIL!?"))
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
                                              (for name = (next element))
                                              (if (typep name 'xml-unquote)
                                                  (collect name)
                                                  (bind ((value (next element)))
                                                    (collect (make-xml-attribute
                                                              (unless-unquote name (name-as-string name))
                                                              (unless-unquote value (princ-to-string value))))))))
           form))))
    (null (simple-reader-error stream "Empty xml tag?"))))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted XML is made of list, xml-syntax-nodes, xml-quasi-quote, xml-unquote recursively

(def ast xml)

(def class* xml-syntax-node (syntax-node)
  ())

(def class* xml-quasi-quote (quasi-quote xml-syntax-node)
  ())

(def (function e) make-xml-quasi-quote (body)
  (make-instance 'xml-quasi-quote :body body))

(def class* xml-unquote (unquote xml-syntax-node)
  ())

(def (function e) make-xml-unquote (form &optional (spliced? #f))
  (make-instance 'xml-unquote :form form :spliced spliced?))

(def (class* e) xml-element (xml-syntax-node)
  ((name)
   (attributes nil)
   (children nil)))

(def (class* e) xml-attribute (xml-syntax-node)
  ((name)
   (value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

(def (function e) make-xml-element (name &optional attributes children)
  (make-instance 'xml-element :name name :attributes attributes :children children))

(def (function e) make-xml-attribute (name value)
  (make-instance 'xml-attribute :name name :value value))

(def (function e) make-xml-text (content)
  (make-instance 'xml-text :content content))

;;;;;;;;;;;;;
;;; Transform

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'xml-quasi-quote fn))

(def special-variable *xml-indent-level* 0)

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/element (node &rest args &key (indent #f) &allow-other-keys)
  (bind ((indent-level
          (when indent
            (list (make-string (* indent *xml-indent-level*) :initial-element #\Space))))(indent-new-line
          (when indent
            (list #\NewLine))))
    (etypecase node
      (function node)
      (string `(,@indent-level ,(escape-as-xml node) ,@indent-new-line))
      (xml-element
       (bind ((attributes (attributes-of node))
              (name (name-of node))
              (children (children-of node)))
         `(,@indent-level
           "<" ,(etypecase name
                           (xml-unquote (make-string-unquote (form-of name)))
                           (unquote name)
                           (string name))
           ,@(when attributes
                   `(" "
                     ,@(typecase attributes
                                 (xml-unquote (list (make-string-unquote `(mapcar 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
                                                                                  ,(form-of attributes)))))
                                 (unquote attributes)
                                 (t (iter (for attribute :in attributes)
                                          (unless (first-iteration-p)
                                            (collect " "))
                                          (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))))))
           ,@(if children
                 `(">" ,@indent-new-line
                       ,@(mapcar (lambda (child)
                                   (bind ((*xml-indent-level* (1+ *xml-indent-level*)))
                                     (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element child args)))
                                 children)
                       (,@indent-level "</" ,(name-of node) ">" ,@indent-new-line))
                 `("/>" ,@indent-new-line)))))
      (xml-text
       (bind ((content (content-of node)))
         `(,@indent-level
           ,(etypecase content
                       (xml-unquote (make-string-quasi-quote (form-of node)))
                       (string (escape-as-xml content)))
           ,@indent-new-line)))
      (xml-quasi-quote
       (make-string-quasi-quote (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element (body-of node) args)))
      (xml-unquote
       (bind ((spliced? (spliced-p node)))
         (make-string-unquote
          (if spliced?
              `(map 'list (lambda (node)
                            ,(wrap-forms-with-bindings
                              (when indent `((*xml-indent-level* ,*xml-indent-level*)))
                              `(transform-quasi-quoted-xml-to-quasi-quoted-string/element node ,@args)))
                    ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                      node (lambda (node)
                             (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element node args))))
              (wrap-forms-with-bindings
               (when indent `((*xml-indent-level* ,*xml-indent-level*)))
               `(transform-quasi-quoted-xml-to-quasi-quoted-string/element
                 ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                   node (lambda (node)
                          (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element node args)))
                 ,@args)))
          spliced?)))
      (quasi-quote
       (if (typep node 'string-quasi-quote)
           (body-of node)
           node))
      (unquote (transform 'quasi-quoted-string node))
      (side-effect node))))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/attribute (node)
  (etypecase node
    (function node)
    (xml-attribute
     (bind ((name (name-of node))
            (value (value-of node)))
       `(,(etypecase name
                     (xml-unquote (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute name))
                     (unquote name)
                     (string name))
          "=\""
          ,(etypecase value
                      (xml-unquote (make-string-unquote
                                    `(escape-as-xml
                                      (princ-to-string
                                       ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                         value #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))))
                      (unquote value)
                      (string (escape-as-xml value)))
          "\"")))
    (xml-quasi-quote
     (make-string-quasi-quote (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
    (xml-unquote
     (bind ((spliced? (spliced-p node)))
       (make-string-unquote
        (if spliced?
            `(iter (for attribute :in-sequence ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                                                 node #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute))
                   (unless (first-iteration-p)
                     (collect " "))
                   (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))
            `(transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
              ,(transform-quasi-quoted-xml-to-quasi-quoted-string/process-unquoted-form
                node #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
        spliced?)))
    (quasi-quote
     (if (typep node 'string-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-string node))))

(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-xml-to-quasi-quoted-string/element input args))
