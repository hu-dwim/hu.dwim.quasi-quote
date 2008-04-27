;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(define-syntax (quasi-quoted-js :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-js-quasi-quote (mapcar 'body-of result))
                                       (first result))))
    (&key (start-character #\J)
          end-character
          (dispatch-character #\#)
          (unquote-character #\,)
          (splice-character #\@)
          (transformation nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     `(transform-js-reader-body ,body ,transformation))
   (lambda (body spliced?)
     `(transform-js-reader-unquote ,body ,spliced?))
   :nested-quasi-quote-wrapper (lambda (body)
                                 `(transform-js-reader-body ,body ,transformation))
   :start-character start-character
   :dispatch-character dispatch-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-js-to-js ()
  (set-quasi-quoted-js-syntax-in-readtable :transformation '(js)))

(define-syntax quasi-quoted-js-to-js-emitting-form ()
  (set-quasi-quoted-js-syntax-in-readtable :transformation '(js-emitting-form)))

(define-syntax quasi-quoted-js-to-string ()
  (set-quasi-quoted-js-syntax-in-readtable :transformation '(quasi-quoted-string string)))

(define-syntax quasi-quoted-js-to-string-emitting-form ()
  (set-quasi-quoted-js-syntax-in-readtable :transformation '(quasi-quoted-string string-emitting-form)))

(define-syntax quasi-quoted-js-to-binary ()
  (set-quasi-quoted-js-syntax-in-readtable :transformation '(quasi-quoted-string quasi-quoted-binary binary)))

(define-syntax quasi-quoted-js-to-binary-emitting-form ()
  (set-quasi-quoted-js-syntax-in-readtable :transformation '(quasi-quoted-string quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-js-to-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-js-syntax-in-readtable :transformation `(quasi-quoted-string quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(def macro transform-js-reader-body (form transformation &environment lexenv)
  (chain-transform transformation (typecase form
                                    (syntax-node form)
                                    (t (make-js-quasi-quote (walk-js form lexenv))))))

(def macro transform-js-reader-unquote (body spliced?)
  ;; A macro to handle the quoted parts when the walker is walking the forms. Kinda like a kludge, but it's not that bad...
  (make-js-unquote body spliced?))

(defwalker-handler transform-js-reader-unquote (form parent env)
  (macroexpand-1 form (cdr env)))

(def function walk-js (form lexenv)
  (with-walker-configuration (:warn-for-undefined-references #f ;; TODO should not need to disable it, but we are far from that for now
                              :function-name?     (fdefinition 'js-function-name?)
                              :macro-name?        (fdefinition 'js-macro-name?)
                              :symbol-macro-name? (fdefinition 'js-symbol-macro-name?)
                              :macroexpand-1      (fdefinition 'js-macroexpand-1))
    (walk-form form nil (make-walk-environment lexenv))))
