;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def class* syntax-node ()
  ())

(def (class* e) quasi-quote (syntax-node)
  ((body)
   (toplevel #f :type boolean)))

(def (class* e) unquote (syntax-node)
  ((form)
   (spliced #f :type boolean)))

(defmethod make-load-form ((self syntax-node) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;; TODO: subclass quasi-quote and unquote for each DSL so asserts can help
;; TODO: DSLs: string, vector, bivalent, XML, XHTML, SQL, JS, typesetting, pdf
;; TODO: continuation support? what about lazyness? what about computed-class?
