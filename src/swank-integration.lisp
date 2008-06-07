;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(register-readtable-for-swank
 '("CL-QUASI-QUOTE") 'setup-readtable)

(dolist (package '("CL-QUASI-QUOTE-JS"
                   "CL-QUASI-QUOTE-XML"
                   "CL-QUASI-QUOTE-PDF"
                   "CL-QUASI-QUOTE-ODF"))
  (awhen (find-package package)
    (register-readtable-for-swank (list package) (find-symbol "SETUP-READTABLE" it))))
