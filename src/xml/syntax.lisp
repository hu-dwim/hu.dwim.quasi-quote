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
          (transformation nil))
  (bind ((original-reader-on-start-character   (multiple-value-list (get-macro-character start-character *readtable*)))
         (original-reader-on-end-character     (when end-character
                                                 (multiple-value-list (get-macro-character end-character *readtable*))))
         (original-reader-on-unquote-character (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-quasi-quote-syntax-in-readtable
     (lambda (body)
       (readtime-chain-transform transformation (make-xml-quasi-quote (parse-xml-reader-body body))))
     (lambda (body spliced?)
       (make-xml-unquote body spliced?))
     :nested-quasi-quote-wrapper (lambda (body)
                                   (parse-xml-reader-body body))
     :start-character start-character
     :end-character end-character
     :unquote-character unquote-character
     :splice-character splice-character
     :readtable-case :preserve
     :toplevel-reader-wrapper (lambda (reader)
                                (lambda (stream char)
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
                                              (return (read stream t nil t))))
                                          (funcall reader stream char)))))))))

(define-syntax quasi-quoted-xml-to-xml ()
  (set-quasi-quoted-xml-syntax-in-readtable :transformation '(xml)))

(define-syntax quasi-quoted-xml-to-xml-emitting-form ()
  (set-quasi-quoted-xml-syntax-in-readtable :transformation '(xml-emitting-form)))

(define-syntax quasi-quoted-xml-to-string ()
  (set-quasi-quoted-xml-syntax-in-readtable :transformation '(quasi-quoted-string string)))

(define-syntax quasi-quoted-xml-to-string-emitting-form ()
  (set-quasi-quoted-xml-syntax-in-readtable :transformation '(quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-xml-to-binary ()
  (set-quasi-quoted-xml-syntax-in-readtable :transformation '(quasi-quoted-string quasi-quoted-binary binary)))

(define-syntax quasi-quoted-xml-to-binary-emitting-form ()
  (set-quasi-quoted-xml-syntax-in-readtable :transformation '(quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-xml-to-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-xml-syntax-in-readtable :transformation `(quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(def (function d) parse-xml-reader-body (form)
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
                                              (for name = (next element))
                                              (if (typep name 'xml-unquote)
                                                  (collect name)
                                                  (bind ((value (next element)))
                                                    (collect (make-xml-attribute
                                                              (unless-unquote name (name-as-string name))
                                                              (unless-unquote value (princ-to-string value))))))))
           form))))
    (null (simple-reader-error nil "Empty xml tag?"))))

(def function name-as-string (name)
  (etypecase name
    (string name)
    (symbol (symbol-name name))))
