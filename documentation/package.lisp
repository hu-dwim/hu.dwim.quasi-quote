;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.quasi-quote.documentation
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.quasi-quote
        :hu.dwim.quasi-quote.test
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui))

#| ;; TODO:
- subclass quasi-quote and unquote for each DSL so asserts can help
- additional DSLs: string, vector, bivalent, XML, XHTML, SQL, JS, typesetting, pdf
- continuation support?
- what about lazyness?
- what about computed-class?

*** js

- introduce a js-special-form that is called on the sexp and rename
  the current definer to walked-js-special-form
|#
