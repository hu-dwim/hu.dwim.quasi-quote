;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-string (&key start-character
                                         end-character
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (destructive-splice-character #\.)
                                         (dispatched-quasi-quote-name "str")
                                         (transformation-pipeline nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (bind ((toplevel? (= 1 *quasi-quote-nesting-level*)))
       `(,(if toplevel? 'string-quasi-quote/toplevel 'string-quasi-quote) ,toplevel? ,body ,transformation-pipeline)))
   (lambda (form modifier)
     `(string-unquote ,form ,modifier))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character
   :dispatched-quasi-quote-name dispatched-quasi-quote-name))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-STRING-TO-~A" name))
                    (&key-position (position '&key args)))
               `(define-syntax ,syntax-name (,@(subseq args 0 (or &key-position (length args)))
                                               &key
                                               (with-inline-emitting #f)
                                               (declarations '())
                                               start-character
                                               end-character
                                               (unquote-character #\,)
                                               (splice-character #\@)
                                               (destructive-splice-character #\.)
                                               (dispatched-quasi-quote-name "str")
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-string-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                               :dispatched-quasi-quote-name dispatched-quasi-quote-name
                                                               :start-character start-character
                                                               :end-character end-character
                                                               :unquote-character unquote-character
                                                               :splice-character splice-character
                                                               :destructive-splice-character destructive-splice-character)))))
  (x string-emitting-form (make-quasi-quoted-string-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name))
  (x binary-emitting-form (make-quasi-quoted-string-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary t
                           :encoding encoding
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name &key (encoding *default-character-encoding*))))

(def (function e) make-quasi-quoted-string-to-form-emitting-transformation-pipeline
    (stream-variable-name &key binary with-inline-emitting (encoding :utf-8) declarations)
  (if binary
      (list (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                           :encoding encoding)
            (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))
      (list (make-instance 'quasi-quoted-string-to-string-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))))

(def reader-stub string-quasi-quote (toplevel? body transformation-pipeline)
  (bind ((expanded-body (recursively-macroexpand-reader-stubs body -environment-))
         (quasi-quote-node (make-string-quasi-quote transformation-pipeline expanded-body)))
    (if toplevel?
        (run-transformation-pipeline quasi-quote-node)
        quasi-quote-node)))

(def reader-stub string-unquote (form spliced?)
  (make-string-unquote form spliced?))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted string is made of character, string, list, string-quasi-quote, string-unquote nodes recursively.

(def ast string)

(def class* string-syntax-node (syntax-node)
  ())

(def (class* e) string-quasi-quote (quasi-quote string-syntax-node)
  ())

(def (function e) make-string-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'string-quasi-quote :transformation-pipeline transformation-pipeline :body body))

(def (class* e) string-unquote (unquote string-syntax-node)
  ())

(def (function e) make-string-unquote (form &optional modifier)
  (make-instance 'string-unquote :form form :modifier modifier))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform to binary-emitting-form

(def class* quasi-quoted-string-transformation-mixin ()
  ((escape-function nil)))

(def method compatible-transformations? and ((a quasi-quoted-string-transformation-mixin)
                                             (b quasi-quoted-string-transformation-mixin))
  (eql (escape-function-of a) (escape-function-of b)))

(def (transformation e) quasi-quoted-string-to-string-emitting-form (quasi-quoted-string-transformation-mixin
                                                                     lisp-form-emitting-transformation)
  ()
  'transform-quasi-quoted-string-to-string-emitting-form)

(def function string-concatenate (elements)
  (bind ((*print-pretty* #f)
         (*print-readably* #f))
    (with-output-to-string (stream)
      (dolist (el elements)
        (etypecase el
          (string (write-string el stream))
          (character (write-char el stream)))))))

(def function write-quasi-quoted-string (node stream &optional escape-function)
  (if escape-function
      (etypecase node
        (character (write-string (funcall escape-function (string node)) stream))
        (string (write-string (funcall escape-function node) stream))
        (list (mapc (lambda (sub-node)
                      (write-quasi-quoted-string sub-node stream))
                    node))
        (delayed-emitting (funcall node)))
      (etypecase node
        (character (write-char node stream))
        (string (write-string node stream))
        (list (mapc (lambda (sub-node)
                      (write-quasi-quoted-string sub-node stream))
                    node))
        (delayed-emitting (funcall node))))
  (values))

(def function make-quasi-quoted-string-emitting-form (node)
  (bind ((stream (stream-variable-name-of *transformation*))
         (runtime-escape-function (awhen (escape-function-of *transformation*)
                                    (ensure-function it)))
         (compile-time-escape-function (or (escape-function-of *transformation*)
                                           #'identity)))
    (etypecase node
      (character `(write-string ,(funcall compile-time-escape-function (string node)) ,stream))
      (string `(write-string ,(funcall compile-time-escape-function node) ,stream))
      (string-unquote
       `(write-quasi-quoted-string
         ,(transform-quasi-quoted-string-to-string-emitting-form node)
         ,stream
         ,runtime-escape-function))
      (side-effect (form-of node)))))

(def function reduce-string-subsequences (sequence)
  (reduce-subsequences sequence
                       (lambda (el) (or (stringp el) (characterp el)))
                       #'string-concatenate))

(def function transform-quasi-quoted-string-to-string-emitting-form (input)
  (transformation-typecase input
    (string-quasi-quote
     (wrap-emitting-forms (mapcar 'make-quasi-quoted-string-emitting-form
                                  (reduce-string-subsequences
                                   (transform-quasi-quoted-string-to-string-emitting-form/flatten-body input)))))
    (string-unquote
     (map-filtered-tree (form-of input) 'string-quasi-quote
                        (lambda (node)
                          (transform-quasi-quoted-string-to-string-emitting-form node))))))

(defun transform-quasi-quoted-string-to-string-emitting-form/flatten-body (node)
  (let (result)
    (labels ((traverse (subtree)
               (when subtree
                 (typecase subtree
                   (cons
                    (traverse (car subtree))
                    (traverse (cdr subtree)))
                   (string-quasi-quote (bind ((nested-node subtree))
                                         (if (compatible-transformation-pipelines?
                                              (transformation-pipeline-of node)
                                              (transformation-pipeline-of nested-node))
                                             ;; if the pipelines are compatible, then just skip over the qq node
                                             ;; and descend into its body as if it never was there...
                                             (traverse (body-of nested-node))
                                             (push (transform nested-node) result))))
                   (t (push subtree result))))))
      (traverse (body-of node)))
    (nreverse result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform to binary-quasi-quote

(def (transformation e) quasi-quoted-string-to-quasi-quoted-binary (quasi-quoted-string-transformation-mixin)
  ((encoding *default-character-encoding*))
  'transform-quasi-quoted-string-to-quasi-quoted-binary)

(def method compatible-transformations? and ((a quasi-quoted-string-to-quasi-quoted-binary)
                                             (b quasi-quoted-string-to-quasi-quoted-binary))
  (eql (encoding-of a) (encoding-of b)))

(def function transform-quasi-quoted-string-to-quasi-quoted-binary (node)
  (bind ((encoding (encoding-of *transformation*))
         (compiletime-escape-function (or (escape-function-of *transformation*)
                                          #'identity)))
    (transformation-typecase node
      (list
       (mapcar (lambda (child)
                 (transform-quasi-quoted-string-to-quasi-quoted-binary child))
               node))
      (character (babel:string-to-octets (funcall compiletime-escape-function (string node)) :encoding encoding))
      (string (babel:string-to-octets (funcall compiletime-escape-function node) :encoding encoding))
      (string-quasi-quote
       (make-binary-quasi-quote (rest (transformation-pipeline-of node))
                                (transform-quasi-quoted-string-to-quasi-quoted-binary (body-of node))))
      (string-unquote
       (make-binary-unquote
        (wrap-runtime-delayed-transformation-form
         `(transform-quasi-quoted-string-to-quasi-quoted-binary
           ,(map-filtered-tree (form-of node) 'string-quasi-quote
                               (lambda (child)
                                 (transform-quasi-quoted-string-to-quasi-quoted-binary child))))))))))
