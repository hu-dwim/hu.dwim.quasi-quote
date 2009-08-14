;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(dolist (package '(:hu.dwim.quasi-quote
                   :hu.dwim.quasi-quote.js
                   :hu.dwim.quasi-quote.xml
                   :hu.dwim.quasi-quote.pdf
                   :hu.dwim.quasi-quote.odf
                   :hu.dwim.quasi-quote.css))
  (register-readtable-for-swank
   (list package)
   (find-symbol "SETUP-READTABLE"
                (or (find-package package)
                    (progn
                      (warn "Package ~S is not loaded yet, so registering hu.dwim.quasi-quote::setup-readtable in swank for it" package)
                      :hu.dwim.quasi-quote)))))
