;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-pdf)

;;;;;;;;;
;;; Parse
;;;
(def special-variable *pdf-quasi-quote-level* 0)

(define-syntax quasi-quoted-pdf (&key (start-character #\[)
                                      (end-character #\])
                                      (unquote-character #\,)
                                      (splice-character #\@)
                                      (transform nil))
  (bind ((original-reader-on-start-character   (multiple-value-list (get-macro-character start-character *readtable*)))
         (original-reader-on-end-character     (multiple-value-list (get-macro-character end-character *readtable*)))
         (original-reader-on-unquote-character (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-macro-character start-character
                         (make-quasi-quoted-pdf-reader original-reader-on-start-character
                                                       original-reader-on-end-character
                                                       original-reader-on-unquote-character
                                                       start-character end-character
                                                       unquote-character
                                                       splice-character
                                                       transform)
                         t
                         *readtable*)))

;; TODO the quasi-quote reader in cl-syntax-sugar should be extended to accomodate this kind of usage.
;; TODO don't forget about the same for xml
(def function make-quasi-quoted-pdf-reader (original-reader-on-start-character
                                             original-reader-on-end-character
                                             original-reader-on-unquote-character
                                             start-character end-character
                                             unquote-character splice-character
                                             transform)
  (labels ((unquote-reader (stream char)
             (declare (ignore char))
             (bind ((*readtable* (copy-readtable))
                    (*pdf-quasi-quote-level* (1- *pdf-quasi-quote-level*))
                    (spliced? (eq (peek-char nil stream t nil t) splice-character)))
               (when spliced?
                 (read-char stream t nil t))
               (assert (<= 0 *pdf-quasi-quote-level*))
               (when (zerop *pdf-quasi-quote-level*)
                 ;; restore the original unquote reader when we are leaving our nesting. this way it's possible
                 ;; to use #\, in its normal meanings when being outside our own nesting levels.
                 (apply 'set-macro-character unquote-character original-reader-on-unquote-character)
                 (apply 'set-macro-character start-character original-reader-on-start-character)
                 (apply 'set-macro-character end-character original-reader-on-end-character))
               (set-macro-character start-character #'toplevel-quasi-quoted-pdf-reader)
               (bind ((body (read stream t nil t)))
                 (make-pdf-unquote body spliced?))))
           (toplevel-quasi-quoted-pdf-reader (stream char)
             (declare (ignore char))
             ;; we must set the syntax on the end char to be like #\)
             ;; until we read out our entire body. this is needed to
             ;; make "<... 5> style inputs work where '5' is not
             ;; separated from '>'.
             (bind ((*pdf-quasi-quote-level* (1+ *pdf-quasi-quote-level*))
                    (cl-quasi-quote::*quasi-quote-level* (1+ cl-quasi-quote::*quasi-quote-level*))
                    (*readtable* (copy-readtable)))
               (set-macro-character unquote-character #'unquote-reader)
               ;; on nested invocations we want to do something else then on the toplevel invocation
               (set-macro-character start-character #'nested-quasi-quoted-pdf-reader)
               (bind ((body (if end-character
                                (bind ((*readtable* (copy-readtable)))
                                  (set-syntax-from-char end-character #\) *readtable*)
                                  (read-delimited-list end-character stream t))
                                (read stream t nil t))))
                 (readtime-chain-transform transform (make-pdf-quasi-quote (parse-pdf-reader-body body))))))
           (nested-quasi-quoted-pdf-reader (stream char)
             (declare (ignore char))
             (parse-pdf-reader-body (if end-character
                                        (read-delimited-list end-character stream t)
                                        (read stream t nil t)))))
    #'toplevel-quasi-quoted-pdf-reader))

(define-syntax quasi-quoted-pdf-to-binary ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary)))

(define-syntax quasi-quoted-pdf-to-binary-emitting-form ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-pdf-to-binary-stream-emitting-form (stream-name)
  (set-quasi-quoted-pdf-syntax-in-readtable :transform `(quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream-name ,stream-name))))

(def function parse-pdf-reader-body (form)
  (if (typep form 'syntax-node)
      form
      (bind ((sexp-parser (gethash (first form) *pdf-ast-node-name->sexp-parser*)))
        (assert sexp-parser)
        (funcall sexp-parser form))))

;;;
;;; Override some parsers where the default expansion from the pdf-ast-node definer is not ok
;;;
(def pdf-ast-node-parser document
  (make-pdf-document (rest -sexp-)))

(def pdf-ast-node-parser array
  (make-instance 'pdf-array :value (mapcar #'parse-into-pdf-syntax-node (rest -sexp-))))

(def pdf-ast-node-parser dictionary
  (parse-dictionary-map (make-instance 'pdf-dictionary) (rest -sexp-)))

(def pdf-ast-node-parser catalog
  (parse-dictionary-map (make-instance 'pdf-catalog) (rest -sexp-)))

(def pdf-ast-node-parser pages
  (parse-dictionary-map (make-instance 'pdf-pages) (rest -sexp-)))

(def pdf-ast-node-parser page
  (parse-dictionary-map (make-instance 'pdf-page) (rest -sexp-)))

(def pdf-ast-node-parser stream
  (make-instance 'pdf-stream :contents (rest -sexp-)))

(def function parse-dictionary-map (dictionary elements)
  (iter
    (with map = (map-of dictionary))
    (while elements)
    (for key = (pop elements))
    (for value = (pop elements))
    (setf key (etypecase key
                (pdf-name key)
                (string (make-pdf-name key))
                (symbol (make-pdf-name key))))
    (setf value (parse-into-pdf-syntax-node value))
    (setf (parent-of value) dictionary)
    (setf (parent-of key) dictionary)
    (setf (gethash key map) value))
  dictionary)

(def function parse-into-pdf-syntax-node (value)
  (if (eq value #t)
      (make-pdf-boolean #t)
      (etypecase value
        (pdf-syntax-node value)
        ((or integer float) (make-pdf-number value))
        (string (make-pdf-string value))
        (null (make-pdf-boolean #f))
        (vector (make-instance 'pdf-array :value (map 'list #'parse-into-pdf-syntax-node value))))))
