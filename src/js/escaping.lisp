;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-js)

(def (function eo) escape-as-js-string (string &optional destination)
  (declare (type string string)
           (type (or null (array character (*))) destination))
  (bind ((result destination))
    (iter (for index :from 0 :below (length string))
          (for character = (aref string index))
          (for character-code = (char-code character))
          (if (or (eql character-code #.(char-code #\\))
                  (eql character-code #.(char-code #\'))
                  (eql character-code #.(char-code #\")))
              (progn
                (unless result
                  (setf result (make-array (floor (* 1.1 (length string)))
                                           :element-type 'character
                                           :adjustable #t
                                           :fill-pointer index))
                  (replace result string :end2 index))
                (vector-push-extend #\\ result)
                (vector-push-extend character result))
              (when result
                (vector-push-extend character result))))
    (if result
        (shrink-vector result (length result))
        string)))
