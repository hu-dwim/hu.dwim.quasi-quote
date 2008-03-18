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
  ((form)))
