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
  (if (typep form 'syntax-node)
      form
      (parse-quasi-quoted-pdf* (pdf-syntax-node-name (first form)) form)))

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

  (:method ((first (eql 'pdf-stream)) whole)
    (make-instance 'pdf-stream :contents (mapcar #'parse-quasi-quoted-pdf (cdr whole))))

  (:method ((first (eql 'pdf-begin-text)) whole)
    (make-instance 'pdf-begin-text))

  (:method ((first (eql 'pdf-end-text)) whole)
    (make-instance 'pdf-end-text))

  (:method ((first (eql 'pdf-set-font)) whole)
    (make-instance 'pdf-set-font
                   :name (second whole)
                   :size (third whole)))

  (:method ((first (eql 'pdf-set-text-position)) whole)
    (make-instance 'pdf-set-text-position
                   :x (second whole)
                   :y (third whole)))

  (:method ((first (eql 'pdf-display-text)) whole)
    (make-instance 'pdf-display-text))

  (:method ((first (eql 'pdf-indirect-object)) whole)
    (make-instance 'pdf-indirect-object
                   :name (second whole)
                   :content (parse-quasi-quoted-pdf (third whole))))

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
    (make-instance 'pdf-root :content (parse-quasi-quoted-pdf (second whole))))

  (:method ((first (eql 'pdf-info)) whole)
    (make-instance 'pdf-info :content (parse-quasi-quoted-pdf (second whole)))))

(def function parse-dictionary-map (dictionary pairs)
  (iter (with map = (map-of dictionary))
        (for (key value) :on pairs :by 'cddr)
        (for parsed-key = (parse-quasi-quoted-pdf key))
        (for parsed-value = (parse-quasi-quoted-pdf value))
        (assert (typep parsed-key 'pdf-name))
        (setf (parent-of parsed-value) dictionary)
        (setf (parent-of parsed-key) dictionary)
        (setf (gethash parsed-key map) parsed-value)))

;;;;;;;
;;; AST

(def ast pdf)

(def special-variable *compile-time-pdf-node-identity-counter* 0)

(def class* pdf-syntax-node (syntax-node parent-mixin)
  ())

(def (class* e) pdf-quasi-quote (quasi-quote pdf-syntax-node)
  ())

(def (function e) make-pdf-quasi-quote (body)
  (make-instance 'pdf-quasi-quote :body body))

(def (class* e) pdf-unquote (unquote pdf-syntax-node)
  ())

(def (function e) make-pdf-unquote (form &optional (spliced? #f))
  (make-instance 'pdf-unquote :form form :spliced spliced?))

(def (class*) pdf-object-identifier (pdf-syntax-node)
  ((node-identity (incf *compile-time-pdf-node-identity-counter*) :type integer)
   (object-id nil :type integer)
   (generation-number 0 :type integer)))

(def (class*) pdf-indirect-object (pdf-object-identifier)
  ((name :type symbol)
   (content :type pdf-syntax-node)))

(def (class*) pdf-indirect-object-reference (pdf-object-identifier)
  ((name nil :type symbol)))

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
  ((value :type list)))

(def (class*) pdf-stream (pdf-syntax-node)
  ((contents :type list)))

(def (class*) pdf-begin-text (pdf-syntax-node)
  ())

(def (class*) pdf-end-text (pdf-syntax-node)
  ())

(def (class*) pdf-set-font (pdf-syntax-node)
  ((name :type string)
   (size :type float)))

(def (class*) pdf-set-text-position (pdf-syntax-node)
  ((x :type float)
   (y :type float)))

(def (class*) pdf-display-text (pdf-syntax-node)
  ())

(def (class*) pdf-dictionary (pdf-syntax-node)
  ((map (make-hash-table) :type hash-table)))

(def (class*) pdf-catalog (pdf-dictionary)
  ())

(def (class*) pdf-pages (pdf-dictionary)
  ())

(def (class*) pdf-page (pdf-dictionary)
  ())

(def (class*) pdf-root (pdf-indirect-object)
  ((content)))

(def (class*) pdf-info (pdf-indirect-object)
  ((content)))

(def (class*) pdf-position-mixin ()
  ((position 0 :type integer)))

(def (class*) pdf-xref-entry (pdf-object-identifier pdf-position-mixin)
  ((free :type boolean)))

(def (class*) pdf-xref-section (pdf-syntax-node)
  ((entries nil :type list)))

(def (class*) pdf-xref (pdf-syntax-node pdf-position-mixin)
  ((sections nil :type list)))

(def (class*) pdf-header (pdf-syntax-node)
  ((version "1.4" :type string)))

(def (class*) pdf-trailer (pdf-syntax-node)
  ((dictionary (make-trailer-dictionary) :type pdf-dictionary)))

(def (class*) pdf-document (pdf-syntax-node)
  ((header (make-instance 'pdf-header) :type pdf-header)
   (trailer (make-instance 'pdf-trailer) :type pdf-trailer)
   (elements :type list)
   (xref (make-instance 'pdf-xref
                        :sections (list (make-instance 'pdf-xref-section
                                                       :entries (list (make-instance 'pdf-xref-entry
                                                                                     :free #t
                                                                                     :object-id 0
                                                                                     :generation-number 65535)))))
         :type pdf-xref)))

(def function make-trailer-dictionary ()
  (bind ((map (make-hash-table)))
    (setf (gethash (make-instance 'pdf-name :value "Root") map)
          (make-pdf-unquote '(root-reference-of *pdf-environment*)))
    (setf (gethash (make-instance 'pdf-name :value "Info") map)
          (make-pdf-unquote '(info-reference-of *pdf-environment*)))
    (setf (gethash (make-instance 'pdf-name :value "Size") map)
          (make-bivalent-unquote '(princ-to-string (compute-xref-size (xref-of *pdf-environment*)))))
    (make-instance 'pdf-dictionary :map map)))

;;;;;;;;;;;;;
;;; Transform

(def special-variable *pdf-environment*)

(def class* pdf-environment ()
  ((object-id-counter 0 :type integer)
   (node-identity-to-object-id (make-hash-table) :type hash-table)
   (xref-position :type integer)
   (xref (make-instance 'pdf-xref) :type pdf-xref)
   (root-reference :type pdf-indirect-object-reference)
   (info-reference :type pdf-indirect-object-reference)))

(def function compute-xref-size (xref)
  ;; TODO: 1+ due to the default xref
  (1+ (iter (for section :in (sections-of xref))
            (sum (length (entries-of section))))))

(def function ensure-pdf-object-id-for-node-identity (node-identity)
  (bind ((map (node-identity-to-object-id-of *pdf-environment*))
         (object-id (gethash node-identity map)))
    (or object-id
        (setf (gethash node-identity map) (incf (object-id-counter-of *pdf-environment*))))))

(def function node-binary-position ())

(def function push-xref-entry (object-id generation-number position)
  (bind ((xref (xref-of *pdf-environment*))
         (section (first (sections-of xref)))
         (entry (when section (first (entries-of section)))))
    (when (or (not entry)
              (and entry
                   (not (= object-id (1+ (object-id-of entry))))))
      (setf section (make-instance 'pdf-xref-section))
      (push section (sections-of xref)))
    (push (make-instance 'pdf-xref-entry
                         :free #f
                         :position position
                         :object-id object-id
                         :generation-number generation-number)
          (entries-of section))
    object-id))

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

    (:method ((node side-effect))
      node)

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
        (etypecase value
          (unquote (recurse value))
          (side-effect (recurse value))
          (integer (princ-to-string value))
          ;; rationals and such are not allowed.
          (t (format nil "~,3F" value)))))

    (:method ((node pdf-name))
      (format nil "/~A" (value-of node)))

    (:method ((node pdf-string))
      (format nil "(~A)" (value-of node)))

    (:method ((node pdf-array))
      (list "["
            (iter (for element :in-sequence (value-of node))
                  (unless (first-iteration-p)
                    (collect #\Space))
                  (collect (recurse element)))
            "]"))

    (:method ((node pdf-stream))
      (with-unique-names (position)
        (bind ((length-node (make-instance 'pdf-indirect-object :content (make-instance 'pdf-number))))
          (setf (value-of (content-of length-node)) (make-bivalent-unquote `(princ-to-string ,position)))
          (make-bivalent-unquote
           `(let (,position)
              ,(make-bivalent-quasi-quote
                (list (format nil "<<~%/Length ")
                      (make-bivalent-unquote
                       `(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent
                         (make-instance 'pdf-indirect-object-reference
                                        :object-id (ensure-pdf-object-id-for-node-identity ,(node-identity-of length-node))
                                        :generation-number ,(generation-number-of length-node))))
                      (format nil "~%>>~%stream~%")
                      (make-side-effect `(setf ,position (binary-position)))
                      (iter (for element :in (contents-of node))
                            (unless (first-iteration-p)
                              (collect #\Space))
                            (collect (recurse element)))
                      (make-side-effect `(setf ,position (- (binary-position) ,position)))
                      (format nil "~%endstream~%")
                      (recurse length-node))))))))

    (:method ((node pdf-begin-text))
      "BT")

    (:method ((node pdf-end-text))
      "ET")

    (:method ((node pdf-set-font))
      (format nil "/~A ~D Tf" (name-of node) (size-of node)))

    (:method ((node pdf-set-text-position))
      (format nil "~D ~D Td" (x-of node) (y-of node)))

    (:method ((node pdf-display-text))
      "Tj")

    (:method ((node pdf-dictionary))
      (list (format nil "<<~%")
            (bind ((class-name (class-name (class-of node))))
              (unless (eq 'pdf-dictionary class-name)
                (format nil "/Type /~A~%" (string-capitalize (subseq (string-downcase (symbol-name class-name)) 4))))) 
            (iter (for (key value) :in-hashtable (map-of node))
                  (assert (typep key 'pdf-name))
                  (collect (recurse key))
                  (collect #\Space)
                  (collect (recurse value))
                  (collect #\NewLine))
            ">>"))

    (:method ((node pdf-indirect-object))
      (list
       (make-bivalent-unquote
        `(princ-to-string
          (push-xref-entry (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))
                           ,(generation-number-of node) (binary-position))))
       (format nil " ~D obj~%" (generation-number-of node))
       (recurse (content-of node))
       (format nil "~%endobj~%")))

    (:method ((node pdf-indirect-object-reference))
      (bind ((name (name-of node)))
        (when name
          (setf node
                (find name (elements-of (find-ancestor node 'pdf-document))
                      :key (lambda (node)
                             (when (and (typep node 'pdf-indirect-object)
                                        (slot-boundp node 'name))
                               (name-of node))))))
        (if (object-id-of node)
            (format nil "~D ~D R" (object-id-of node) (generation-number-of node))
            (list
             (make-bivalent-unquote `(princ-to-string (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))))
             (format nil " ~D R" (generation-number-of node))))))

    (:method ((node pdf-root))
      (list (call-next-method)
            (make-side-effect `(setf (root-reference-of *pdf-environment*)
                                     (make-instance 'pdf-indirect-object-reference
                                                    :object-id (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))
                                                    :generation-number ,(generation-number-of node))))))

    (:method ((node pdf-info))
      (list (call-next-method)
            (make-side-effect `(setf (info-reference-of *pdf-environment*)
                                     (make-instance 'pdf-indirect-object-reference
                                                    :object-id (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))
                                                    :generation-number ,(generation-number-of node))))))

    (:method ((node pdf-xref-entry))
      (format nil "~10,'0D ~5,'0D ~A ~%" (position-of node) (generation-number-of node)
              (if (free-p node) "f" "n")))
    
    (:method ((node pdf-xref-section))
      (bind ((entries (entries-of node)))
        (list
         (format nil "~D ~D~%" (object-id-of (last-elt entries)) (length entries))
         (mapcar #'recurse (reverse entries)))))

    (:method ((node pdf-xref))
      (list (make-side-effect `(setf (xref-position-of *pdf-environment*) (binary-position)))
            (format nil "xref~%")
            (mapcar #'recurse (reverse (sections-of node)))
            (make-bivalent-unquote '(mapcar 'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (sections-of (xref-of *pdf-environment*))))))

    (:method ((node pdf-header))
      (format nil "%PDF-~A~%%Non ASCII marker: í~%" (version-of node)))

    (:method ((node pdf-trailer))
      (list (format nil "trailer~%")
            (recurse (dictionary-of node))
            (format nil "~%startxref~%")
            (make-bivalent-unquote '(princ-to-string (xref-position-of *pdf-environment*)))
            (format nil "~%%%EOF~%")))

    (:method ((node pdf-document))
      (list
       (recurse (header-of node))
       (mapcar #'recurse (elements-of node))
       (recurse (xref-of node))
       (recurse (trailer-of node))))))

(def method transform ((to (eql 'quasi-quoted-bivalent)) (input pdf-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent input))

;; TODO: how to factor this into emit?
(def (function e) emit-pdf (node &optional stream)
  (bind ((*pdf-environment* (make-instance 'pdf-environment)))
    (emit node stream)))
