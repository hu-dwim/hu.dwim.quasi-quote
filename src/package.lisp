;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-quasi-quote
  (:nicknames :qq)

  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :defclass-star
        :closer-mop
        :cl-def
        :cl-syntax-sugar)

  (:export))

(in-package :cl-quasi-quote)

(defun transform-function-definer-options (options)
  (if cl-quasi-quote-system:*load-as-production-p*
      options
      (remove-from-plist options :inline :optimize)))
