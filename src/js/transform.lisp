;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def function transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'js-quasi-quote fn))

(def special-variable *js-indent* 0)
(def special-variable *js-indent-level* 0)

(def function transform-quasi-quoted-js-to-quasi-quoted-string (node)
  (etypecase node
    (function       node)
    (string         (bind ((indent (when *js-indent*
                                     (list (make-spaces (* *js-indent* *js-indent-level*))))))
                      `(,@indent ,(escape-as-js node) #\NewLine)))
    (integer        (princ-to-string node))
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
          (when *js-indent* `((*js-indent-level* ,*js-indent-level*)))
          `(transform-quasi-quoted-js-to-quasi-quoted-string
            ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
              node (lambda (node)
                     (transform-quasi-quoted-js-to-quasi-quoted-string node))))))
     spliced?)))

(def method transform ((to (eql 'quasi-quoted-string)) (input js-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-js-to-quasi-quoted-string input args))
