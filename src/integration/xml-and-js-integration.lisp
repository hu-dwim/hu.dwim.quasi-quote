;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def method compatible-transformations? ((a cl-quasi-quote-xml::quasi-quoted-xml-to-quasi-quoted-string) a-next a-rest
                                         (b cl-quasi-quote-js::quasi-quoted-js-to-quasi-quoted-string) b-next b-rest)
  (compatible-transformations? a-next (first a-rest) (rest a-rest)
                               b-next (first b-rest) (rest b-rest)))
