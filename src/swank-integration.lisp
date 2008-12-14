;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(dolist (package '("CL-QUASI-QUOTE"
                   "CL-QUASI-QUOTE-JS"
                   "CL-QUASI-QUOTE-XML"
                   "CL-QUASI-QUOTE-PDF"
                   "CL-QUASI-QUOTE-ODF"))
  (register-readtable-for-swank
   (list package)
   (find-symbol "SETUP-READTABLE"
                (or (find-package package)
                    (progn
                      (warn "Package ~S is not loaded yet, so registering cl-quasi-quote::setup-readtable in swank for it" package)
                      "CL-QUASI-QUOTE")))))
