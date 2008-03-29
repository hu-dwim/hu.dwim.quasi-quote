;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(def special-variable *xml-quasi-quote-level*)

(define-syntax (quasi-quoted-xml :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-instance 'xml-quasi-quote :body (mapcar 'body-of result))
                                       (first result))))
    (&key (open-bracket-character #\<)
          (close-bracket-character #\>)
          (unquote-character #\,)
          (splice-character #\@))
  (bind ((original-reader-on-open-bracket-character (multiple-value-list (get-macro-character open-bracket-character *readtable*)))
         (original-reader-on-unquote-character      (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-macro-character open-bracket-character
                         (make-quasi-quoted-xml-reader original-reader-on-open-bracket-character
                                                       original-reader-on-unquote-character
                                                       open-bracket-character close-bracket-character
                                                       unquote-character
                                                       splice-character)
                         t
                         *readtable*)))

(def function make-quasi-quoted-xml-reader (original-reader-on-open-bracket-character
                                            original-reader-on-unquote-character
                                            open-bracket-character close-bracket-character
                                            unquote-character splice-character)
  (declare (ignore original-reader-on-open-bracket-character))
  (labels ((unquote-reader (stream char)
             (declare (ignore char))
             (bind ((*readtable* (copy-readtable))
                    (*xml-quasi-quote-level* (1- *xml-quasi-quote-level*))
                    (spliced? (eq (peek-char nil stream t nil t) splice-character)))
               (when spliced?
                 (read-char stream t nil t))
               (assert (<= 0 *xml-quasi-quote-level*))
               (when (zerop *xml-quasi-quote-level*)
                 ;; restore the original unquote reader when we are leaving our nesting. this way it's possible
                 ;; to use #\, in its normal meanings when being outside our own nesting levels.
                 (apply 'set-macro-character unquote-character original-reader-on-unquote-character))
               (bind ((body (read stream t nil t)))
                 (make-instance 'xml-unquote :form body :spliced spliced?))))
           (toplevel-quasi-quoted-xml-reader (stream char)
             (declare (ignore char))
             (bind ((*xml-quasi-quote-level* (if (boundp '*xml-quasi-quote-level*)
                                                 (1+ *xml-quasi-quote-level*)
                                                 1))
                    (*readtable* (copy-readtable)))
               (set-macro-character unquote-character #'unquote-reader)
               ;; on nested invocations we want to do something else then on the toplevel invocation
               (set-macro-character open-bracket-character #'nested-quasi-quoted-xml-reader)
               (bind ((next-char (peek-char nil stream t :eof t))
                      (body (if (or (alphanumericp next-char)
                                    (char= unquote-character next-char))
                                (progn
                                  ;; we must set the syntax on the end char to be like #\)
                                  ;; until we read out our entire body. this is needed to
                                  ;; make "<... 5> style inputs work where '5' is not
                                  ;; separated from '>'.
                                  (bind ((*readtable* (copy-readtable)))
                                    (set-syntax-from-char close-bracket-character #\) *readtable*)
                                    (read-delimited-list close-bracket-character stream t)))
                                ;; seems like we have a standalone #\<, read it as the common-lisp:< symbol
                                (return-from toplevel-quasi-quoted-xml-reader 'common-lisp:<))))
                 (make-instance 'xml-quasi-quote :body (process-xml-reader-body body)))))
           (nested-quasi-quoted-xml-reader (stream char)
             (declare (ignore char))
             (process-xml-reader-body (read-delimited-list close-bracket-character stream t))))
    #'toplevel-quasi-quoted-xml-reader))

(def function process-xml-reader-body (form)
  (bind ((name (pop form))
         (attributes (pop form)))
    (unless name
      (simple-reader-error "Syntax error in XML syntax, node name is NIL!?"))
    (macrolet ((unless-unquote (value &body forms)
                 (once-only (value)
                   `(if (typep ,value 'xml-unquote)
                        ,value
                        (progn
                          ,@forms)))))
      (unless-unquote attributes
        (when (typep attributes 'xml-syntax-node)
          (push attributes form)
          (setf attributes nil)))
      (make-instance 'xml-element
                     :name (name-as-string name)
                     :attributes (unless-unquote attributes
                                   (iter (generate element :in attributes)
                                         (for name = (next element))
                                         (if (typep name 'xml-unquote)
                                             (collect name)
                                             (bind ((value (next element)))
                                               (collect (make-instance 'xml-attribute
                                                                       :name (unless-unquote name (name-as-string name))
                                                                       :value (unless-unquote value (princ-to-string value))))))))
                     :children form))))

(def (function e) with-quasi-quoted-xml-to-binary-syntax (&key stream)
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'quasi-quoted-string 'quasi-quoted-binary
                                        `(binary-emitting-form ,@(when stream
                                                                       `(:stream ,stream)))))

(def (function e) with-quasi-quoted-xml-to-string-syntax ()
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-xml 'quasi-quoted-string 'string-emitting-form))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted XML is made of list, xml-syntax-nodes, xml-quasi-quote, xml-unquote recursively

(def ast xml)

(def class* xml-syntax-node (syntax-node)
  ())

(def class* xml-quasi-quote (quasi-quote xml-syntax-node)
  ())

(def class* xml-unquote (unquote xml-syntax-node)
  ())

(def (class* e) xml-element (xml-syntax-node)
  ((name)
   (attributes nil)
   (children nil)))

(def (class* e) xml-attribute (xml-syntax-node)
  ((name)
   (value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

(def (function e) make-xml-attribute (name value)
  (make-instance 'xml-attribute :name name :value value))

;;;;;;;;;;;;;
;;; Transform

(def method transform ((to (eql 'string)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string (transform 'quasi-quoted-string input) args))

(def method transform ((to (eql 'string-emitting-form)) (input xml-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'string-emitting-form (transform 'quasi-quoted-string input) args))

(def function process-xml-unqoute-form (node fn)
  (map-tree (form-of node)
            (lambda (form)
              (if (typep form 'xml-quasi-quote)
                  (funcall fn form)
                  form))))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/element (node)
  (etypecase node
    (function node)
    (string node)
    (integer (princ-to-string node))
    (xml-element
     (bind ((attributes (attributes-of node))
            (children (children-of node)))
       `("<" ,(name-of node)
             ,@(when attributes
                 `(" "
                   ,@(if (typep attributes 'xml-unquote)
                         (list (make-string-unquote `(mapcar 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
                                                             ,(form-of attributes))))
                         (iter (for attribute :in attributes)
                               (unless (first-iteration-p)
                                 (collect " "))
                               (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute))))))
             ,(if children
                  `(">"
                    ,@(mapcar #'transform-quasi-quoted-xml-to-quasi-quoted-string/element children)
                    ("</" ,(name-of node) ">"))
                  "/>"))))
    (xml-text
     (bind ((content (content-of node)))
       (if (typep content 'xml-unquote)
           (transform-quasi-quoted-xml-to-quasi-quoted-string/element content)
           content)
       ;; TODO: escaping
       #+nil
       ("<!CDATA[["
        ,content
        "]]>")))
    (xml-quasi-quote
     (make-instance 'string-quasi-quote
                    :body (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/element)))
    (xml-unquote
     (make-string-unquote `(transform-quasi-quoted-xml-to-quasi-quoted-string/element
                            ,(process-xml-unqoute-form node #'transform-quasi-quoted-xml-to-quasi-quoted-string/element))))))

(def function transform-quasi-quoted-xml-to-quasi-quoted-string/attribute (node)
  (etypecase node
    (function node)
    (string node)
    (integer (princ-to-string node))
    (xml-attribute
     (bind ((name (name-of node))
            (value (value-of node)))
       `(,(if (typep name 'xml-unquote)
              (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute name)
              name)
          "=\""
          ;; TODO: escaping
          ,(if (typep value 'xml-unquote)
               (make-string-unquote `(princ-to-string
                                      ,(process-xml-unqoute-form
                                        value 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
               (princ-to-string value))
          "\"")))
    (xml-quasi-quote
     (make-instance 'string-quasi-quote
                    :body (map-tree (body-of node) #'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
    (xml-unquote
     (bind ((spliced? (spliced-p node)))
       (make-string-unquote
        (if spliced?
            `(iter (for attribute :in-sequence ,(process-xml-unqoute-form node
                                                                          'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute))
                   (unless (first-iteration-p)
                     (collect " "))
                   (collect (transform-quasi-quoted-xml-to-quasi-quoted-string/attribute attribute)))
            `(transform-quasi-quoted-xml-to-quasi-quoted-string/attribute
              ,(process-xml-unqoute-form node 'transform-quasi-quoted-xml-to-quasi-quoted-string/attribute)))
        spliced?)))))

(def method transform ((to (eql 'quasi-quoted-string)) (input xml-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-xml-to-quasi-quoted-string/element input))
