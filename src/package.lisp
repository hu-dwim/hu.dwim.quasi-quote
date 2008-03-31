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

  (:shadow #:quote)

  (:export))
