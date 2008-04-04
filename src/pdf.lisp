;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse
;;;
;;; http://www.mactech.com/articles/mactech/Vol.15/15.09/PDFIntro/

(define-syntax quasi-quoted-pdf (&key (quasi-quote-character #\[)
                                      (quasi-quote-end-character #\])
                                      (unquote-character #\,)
                                      (splice-character #\@)
                                      (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-pdf-quasi-quote (parse-quasi-quoted-pdf body)))))
   (lambda (form spliced)
     (make-pdf-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-pdf-to-binary ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary)))

(define-syntax quasi-quoted-pdf-to-binary-emitting-form ()
  (set-quasi-quoted-pdf-syntax-in-readtable :transform '(quasi-quoted-bivalent quasi-quoted-binary binary-emitting-form)))

(define-syntax quasi-quoted-pdf-to-binary-stream-emitting-form (stream)
  (set-quasi-quoted-pdf-syntax-in-readtable :transform `(quasi-quoted-bivalent quasi-quoted-binary (binary-emitting-form :stream ,stream))))

(def function pdf-syntax-node-name (name)
  (format-symbol (find-package :cl-quasi-quote) "PDF-~A" name))

(def function parse-quasi-quoted-pdf (form)
  (bind ((*pdf-object-id* 0))
    (if (typep form 'syntax-node)
        form
        (parse-quasi-quoted-pdf* (pdf-syntax-node-name (first form)) form))))

(defgeneric parse-quasi-quoted-pdf* (first whole)
  (:method ((first (eql 'pdf-document)) whole)
    (make-instance 'pdf-document
                   :elements (mapcar #'parse-quasi-quoted-pdf (cdr whole))))

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
    (make-instance 'pdf-array :value (mapcar #'parse-quasi-quoted-pdf (cdr whole))))

  (:method ((first (eql 'pdf-indirect-object)) whole)
    (make-instance 'pdf-indirect-object :content (parse-quasi-quoted-pdf (second whole))))

  (:method ((first (eql 'pdf-dictionary)) whole)
    (bind ((dictionary (make-instance 'pdf-dictionary))
           (map (map-of dictionary)))
      (iter (for (key value) :on (cdr whole) :by 'cddr)
            (for parsed-key = (parse-quasi-quoted-pdf key))
            (assert (typep parsed-key 'pdf-name))
            (setf (gethash parsed-key map) (parse-quasi-quoted-pdf value)))
      dictionary)))

;;;;;;;
;;; AST

(def ast pdf)

(def special-variable *pdf-object-id*)

(def class* pdf-syntax-node (syntax-node)
  ((parent :type syntax-node)
   (position 0 :type integer)))

(def constructor pdf-syntax-node ()
  (iter (with class = (class-of self))
        (for slot :in (class-slots class))
        (when (slot-boundp-using-class class self slot)
          (bind ((value (slot-value-using-class class self slot)))
            (when (typep value 'pdf-syntax-node)
              (setf (parent-of value) self))))))

(def (class* e) pdf-quasi-quote (quasi-quote pdf-syntax-node)
  ())

(def (function e) make-pdf-quasi-quote (body)
  (make-instance 'pdf-quasi-quote :body body))

(def (class* e) pdf-unquote (unquote pdf-syntax-node)
  ())

(def (function e) make-pdf-unquote (form &optional (spliced? #f))
  (make-instance 'pdf-unquote :form form :spliced spliced?))

(def (class*) pdf-object-identifier (pdf-syntax-node)
  ((object-id (incf *pdf-object-id*) :type integer)
   (generation-number 0 :type integer)))

(def (class*) pdf-indirect-object (pdf-object-identifier)
  ((content :type pdf-syntax-node)))

(def (class*) pdf-indirect-object-reference (pdf-object-identifier)
  ())

(def (class*) pdf-null (pdf-syntax-node)
  ())

(def (class*) pdf-boolean (pdf-syntax-node)
  ((value :type boolean)))

(def (class*) pdf-number (pdf-syntax-node)
  ((value :type number)))

(def (class*) pdf-string (pdf-syntax-node)
  ((value :type string)))

(def (class*) pdf-name (pdf-syntax-node)
  ((value :type string)))

(def (class*) pdf-array (pdf-syntax-node)
  ((value :type sequence)))

(def (class*) pdf-dictionary (pdf-syntax-node)
  ((map (make-hash-table) :type hash-table)))

(def (class*) pdf-page (pdf-syntax-node)
  ())

(def (class*) pdf-xref-entry (pdf-object-identifier)
  ((free :type boolean)))

(def (class*) pdf-xref-section (pdf-syntax-node)
  ((entries :type list)))

(def (class*) pdf-xref (pdf-syntax-node)
  ((sections (list (make-instance 'pdf-xref-section
                                  :entries (list (make-instance 'pdf-xref-entry
                                                                :free #t
                                                                :object-id 0
                                                                :generation-number 65535))))
             :type list)))

(def (class*) pdf-header (pdf-syntax-node)
  ((version "1.4" :type string)))

(def (class*) pdf-trailer (pdf-syntax-node)
  ((dictionary (make-instance 'pdf-dictionary) :type pdf-dictionary)))

(def (class*) pdf-document (pdf-syntax-node)
  ((header (make-instance 'pdf-header) :type pdf-header)
   (trailer (make-instance 'pdf-trailer) :type pdf-trailer)
   (elements :type list)
   (xref (make-instance 'pdf-xref) :type pdf-xref)))

;;;;;;;;;;;;;
;;; Transform

(flet ((recurse (node)
         (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent node)))
  (declare (inline recurse))
  (defgeneric transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (node)
    (:method ((node function))
      node)

    (:method ((node quasi-quote))
      (if (typep node 'bivalent-quasi-quote)
          (body-of node)
          node))
  
    (:method ((node unquote))
      (transform 'quasi-quoted-binary node))

    (:method ((node pdf-quasi-quote))
      (make-bivalent-quasi-quote (recurse (body-of node))))

    (:method ((node pdf-unquote))
      (make-bivalent-unquote
       `(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent
         ,(map-filtered-tree (form-of node) 'pdf-quasi-quote #'recurse))))

    (:method ((node pdf-null))
      "null")

    (:method ((node pdf-boolean))
      (if (value-p node)
          "true"
          "false"))

    (:method ((node pdf-number))
      (bind ((value (value-of node)))
        (if (integerp value)
            (princ-to-string value)
            ;; rationals and such are not allowed.
            (format nil "~,3F" value))))

    (:method ((node pdf-name))
      (format nil "/~A" (value-of node)))

    (:method ((node pdf-string))
      (format nil "( ~A )" (value-of node)))

    (:method ((node pdf-array))
      (list "[ "
            (iter (for element :in-sequence (value-of node))
                  (unless (first-iteration-p)
                    (collect #\Space))
                  (collect (recurse element)))
            " ]"))

    (:method ((node pdf-dictionary))
      (list "<<"
            (iter (for (key value) :in-hashtable (map-of node))
                  (assert (typep key 'pdf-name))
                  (collect #\Space)
                  (collect (recurse key))
                  (collect #\Space)
                  (collect (recurse value)))
            " >>"))

    (:method ((node pdf-indirect-object))
      (list
       (format nil "~D ~D obj~%" (object-id-of node) (generation-number-of node))
       (recurse (content-of node))
       (format nil "~%endobj")))

    (:method ((node pdf-indirect-object-reference))
      (format nil "~D ~D R" (object-id-of node) (generation-number-of node)))

    (:method ((node pdf-xref-entry))
      (format nil "~10,'0D ~5,'0D ~A ~%" (object-id-of node) (generation-number-of node)
              (if (free-p node) "f" "n")))
    
    (:method ((node pdf-xref-section))
      (bind ((entries (entries-of node)))
        (list
         (format nil "~D ~D~%" (object-id-of (first entries)) (length entries))
         (mapcar #'recurse entries))))

    (:method ((node pdf-xref))
      (list (format nil "~%xref~%")
            (mapcar #'recurse (sections-of node))))

    (:method ((node pdf-header))
      (format nil "%PDF-~A~%%Non ASCII marker: í~%" (version-of node)))

    (:method ((node pdf-trailer))
      (list (format nil "trailer~%")
            (recurse (dictionary-of node))
            (format nil "~%startxref~%~D~%%%EOF" (position-of (xref-of (parent-of node))))))

    (:method ((node pdf-document))
      (list
       (recurse (header-of node))
       (mapcar #'recurse (elements-of node))
       (recurse (xref-of node))
       (recurse (trailer-of node))))))

(def method transform ((to (eql 'quasi-quoted-bivalent)) (input pdf-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent input))
