;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

(def (constant :test #'equalp) +character-code->xml-escaped-entity+
  (bind ((result (make-array 256 :initial-element nil)))
    (iter (for (character entity) :in '((#\< "&lt;")
                                        (#\> "&gt;")
                                        (#\& "&amp;")
                                        (#\" "&quot;")))
          (setf (aref result (char-code character)) entity))
    result))

(def (function o) escape-as-xml (string &optional destination)
  (declare (type string string)
           (type (or null (array character (*))) destination))
  (bind ((result destination))
    (iter (for index :from 0 :below (length string))
          (for character = (aref string index))
          (for character-code = (char-code character))
          (for entity = (when (< character-code #.(length +character-code->xml-escaped-entity+))
                          (aref #.+character-code->xml-escaped-entity+ character-code)))
          (declare (type fixnum index))
          (if entity
              (progn
                (unless result
                  (setf result (make-array (floor (* 1.1 (length string)))
                                           :element-type 'character
                                           :adjustable #t
                                           :fill-pointer index))
                  (replace result string :end2 index))
                (vector-extend entity result))
              (when result
                (vector-push-extend character result))))
    (or result (shrink-vector string (length string)))))
