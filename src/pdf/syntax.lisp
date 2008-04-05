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

(define-syntax quasi-quoted-pdf (&key (quasi-quote-character #\[)
                                      (quasi-quote-end-character #\])
                                      (unquote-character #\,)
                                      (splice-character #\@)
                                      (transform nil))
  (bind ((original-reader-on-quasi-quote-character     (multiple-value-list (get-macro-character quasi-quote-character *readtable*)))
         (original-reader-on-quasi-quote-end-character (multiple-value-list (get-macro-character quasi-quote-end-character *readtable*)))
         (original-reader-on-unquote-character         (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-macro-character quasi-quote-character
                         (make-quasi-quoted-pdf-reader original-reader-on-quasi-quote-character
                                                       original-reader-on-quasi-quote-end-character
                                                       original-reader-on-unquote-character
                                                       quasi-quote-character quasi-quote-end-character
                                                       unquote-character
                                                       splice-character
                                                       transform)
                         t
                         *readtable*))

  #+nil ;; TODO was. delme eventyually...
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((cl-quasi-quote::*quasi-quote-level* (1+ cl-quasi-quote::*quasi-quote-level*)))
       (readtime-chain-transform transform (make-pdf-quasi-quote (parse-pdf-reader-body body)))))
   (lambda (form spliced)
     (make-pdf-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

;; TODO the quasi-quote reader in cl-syntax-sugar should be extended to accomodate this kind of usage.
;; TODO don't forget about the same for xml
(def function make-quasi-quoted-pdf-reader (original-reader-on-quasi-quote-character
                                             original-reader-on-quasi-quote-end-character
                                             original-reader-on-unquote-character
                                             quasi-quote-character quasi-quote-end-character
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
                 (apply 'set-macro-character quasi-quote-character original-reader-on-quasi-quote-character)
                 (apply 'set-macro-character quasi-quote-end-character original-reader-on-quasi-quote-end-character))
               (set-macro-character quasi-quote-character #'toplevel-quasi-quoted-pdf-reader)
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
               (set-macro-character quasi-quote-character #'nested-quasi-quoted-pdf-reader)
               (bind ((body (if quasi-quote-end-character
                                (bind ((*readtable* (copy-readtable)))
                                  (set-syntax-from-char quasi-quote-end-character #\) *readtable*)
                                  (read-delimited-list quasi-quote-end-character stream t))
                                (read stream t nil t))))
                 (readtime-chain-transform transform (make-pdf-quasi-quote (parse-pdf-reader-body body))))))
           (nested-quasi-quoted-pdf-reader (stream char)
             (declare (ignore char))
             (parse-pdf-reader-body (if quasi-quote-end-character
                                        (read-delimited-list quasi-quote-end-character stream t)
                                        (read stream t nil t)))))
    #'toplevel-quasi-quoted-pdf-reader))

(define-syntax quasi-quoted-pdf-to-binary ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary)))

(define-syntax quasi-quoted-pdf-to-binary-emitting-form ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-pdf-to-binary-stream-emitting-form (stream)
  (set-quasi-quoted-pdf-syntax-in-readtable :transform `(quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream ,stream))))

(def function parse-pdf-reader-body (form)
  (if (typep form 'syntax-node)
      form
      (parse-pdf-reader-body* (pdf-syntax-node-name (first form)) form)))

(defgeneric parse-pdf-reader-body* (first whole)
  (:method ((first (eql 'pdf-document)) whole)
    (make-instance 'pdf-document
                   :elements (mapcar #'parse-pdf-reader-body (cdr whole))))

  (:method ((first (eql 'pdf-null)) whole)
    (make-instance 'pdf-null))

  (:method ((first (eql 'pdf-boolean)) whole)
    (make-instance 'pdf-boolean :value (second whole)))

  (:method ((first (eql 'pdf-number)) whole)
    (make-instance 'pdf-number :value (second whole)))

  (:method ((first (eql 'pdf-name)) whole)
    (make-instance 'pdf-name :value (princ-to-string (second whole))))

  (:method ((first (eql 'pdf-string)) whole)
    (make-instance 'pdf-string :value (second whole)))

  (:method ((first (eql 'pdf-array)) whole)
    (make-instance 'pdf-array :value (mapcar #'parse-pdf-reader-body (cdr whole))))

  (:method ((first (eql 'pdf-stream)) whole)
    (make-instance 'pdf-stream :contents (mapcar #'parse-pdf-reader-body (cdr whole))))

  (:method ((first (eql 'pdf-begin-text)) whole)
    (make-instance 'pdf-begin-text))

  (:method ((first (eql 'pdf-end-text)) whole)
    (make-instance 'pdf-end-text))

  (:method ((first (eql 'pdf-set-font)) whole)
    (make-instance 'pdf-set-font
                   :name (second whole)
                   :size (third whole)))

  (:method ((first (eql 'pdf-move-text)) whole)
    (make-instance 'pdf-move-text
                   :x (second whole)
                   :y (third whole)))

  (:method ((first (eql 'pdf-display-text)) whole)
    (make-instance 'pdf-display-text))

  (:method ((first (eql 'pdf-indirect-object)) whole)
    (make-instance 'pdf-indirect-object
                   :name (second whole)
                   :content (parse-pdf-reader-body (third whole))))

  (:method ((first (eql 'pdf-indirect-object-reference)) whole)
    (make-instance 'pdf-indirect-object-reference
                   :name (second whole)))

  (:method ((first (eql 'pdf-dictionary)) whole)
    (bind ((dictionary (make-instance 'pdf-dictionary)))
      (parse-dictionary-map dictionary (cdr whole))
      dictionary))

  (:method ((first (eql 'pdf-catalog)) whole)
    (bind ((catalog (make-instance 'pdf-catalog)))
      (parse-dictionary-map catalog (cdr whole))
      catalog))

  (:method ((first (eql 'pdf-pages)) whole)
    (bind ((pages (make-instance 'pdf-pages)))
      (parse-dictionary-map pages (cdr whole))
      pages))

  (:method ((first (eql 'pdf-page)) whole)
    (bind ((page (make-instance 'pdf-page)))
      (parse-dictionary-map page (cdr whole))
      page))

  (:method ((first (eql 'pdf-root)) whole)
    (make-instance 'pdf-root :content (parse-pdf-reader-body (second whole))))

  (:method ((first (eql 'pdf-info)) whole)
    (make-instance 'pdf-info :content (parse-pdf-reader-body (second whole)))))

(def function parse-dictionary-map (dictionary pairs)
  (iter (with map = (map-of dictionary))
        (for (key value) :on pairs :by 'cddr)
        (for parsed-key = (parse-pdf-reader-body key))
        (for parsed-value = (parse-pdf-reader-body value))
        (assert (typep parsed-key 'pdf-name))
        (setf (parent-of parsed-value) dictionary)
        (setf (parent-of parsed-key) dictionary)
        (setf (gethash parsed-key map) parsed-value)))

