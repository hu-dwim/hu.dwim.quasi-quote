;;;;;;;;;;;
;;; Package

(defpackage :cl-present-class
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :closer-mop
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-xml
        :ucw))

(in-package :cl-present-class)

(enable-sharp-boolean-syntax)
(enable-quasi-quoted-xml-to-binary-emitting-form-syntax '*http-stream*)

;; tell Swank which readtable to use when C-c C-c'ing
(cl-syntax-sugar:register-readtable-for-swank
 "CL-PRESENT-CLASS" (lambda ()
                      (enable-sharp-boolean-syntax)
                      (enable-quasi-quoted-xml-to-binary-emitting-form-syntax '*http-stream*)))

(defvar *http-stream*)

;;;;;;;;;;;;;;;;;;;;;;;
;;; The class inspector

(defun present-class (class)
  (let ((class-name (fully-qualified-symbol-name (class-name class))))
    <html
     <head
      <title ,class-name>>
     <body (:style "font-size: small;")
           <h2 "The class " <i ,class-name> " is an instance of " ,(present-class-reference (class-of class) #f)>
           <p ,(present-documentation class)>
           ,(present-direct-superclasses class)
           ,(present-direct-subclasses class)
           ,(present-class-precedence-list class)
           ,(present-direct-slots class)
           ,(present-effective-slots class)>>))

(defun present-direct-superclasses (class)
  <div
   <h3 "Direct super classes">
   ,(present-class-references (class-direct-superclasses class))>)

(defun present-direct-subclasses (class)
  <div
   <h3 "Direct sub classes">
   ,(present-class-references (class-direct-subclasses class))>)

(defun present-class-precedence-list (class)
  <div
   <h3 "Class precedence list">
   ,(present-class-references (class-precedence-list class))>)

(defun present-class-references (classes)
  (if classes
      <ul
       ,@(mapcar (lambda (class)
                   <li ,(present-class-reference class)>)
                 classes)>
      <span "There are none">))

(defun present-class-reference (class &optional (documentation #t))
  <span
   <a (:href ,(arnesi:escape-as-uri (class-file-name class))) ,(fully-qualified-symbol-name (class-name class))>
   <br>
   ,@(when documentation
      (list (present-documentation class)))>)

(defun present-direct-slots (class)
  <div
   <h3 "Direct slots">
   ,(present-slots (class-direct-slots class))>)

(defun present-effective-slots (class)
  <div
   <h3 "Effective slots">
   ,(present-slots (class-slots class))>)

(defun present-slots (slots)
  (if slots
      <table (:style "font-size: small;")
        <thead (:bgcolor "#888888")
               <td <i "Name">>
               <td <i "Type">>
               <td <i "Readers">>
               <td <i "Writers">>>
        ,@(iter (for index :from 0)
                (for slot :in slots)
                (appending (present-slot slot index)))>
      <span "There are none">))

(defun present-slot (slot index)
  (bind ((color (if (oddp index) "#eeffbb" "#ffffff")))
    (list
     <tr (:bgcolor ,color)
         <td <b ,(fully-qualified-symbol-name (slot-definition-name slot))>>
         <td ,(princ-to-string (slot-definition-type slot))>
         <td ,(princ-to-string (slot-definition-readers slot))>
         <td ,(princ-to-string (slot-definition-writers slot))>>
     <tr (:bgcolor ,color)
         <td (:colspan 5)
             ,(present-documentation slot)>>)))

(defun present-documentation (what)
  (bind ((content (documentation what t)))
    (or content +void+)))

;;;;;;;;;;;;;;;;;;;;;
;;; Present in a file

(defun fully-qualified-symbol-name (symbol)
  (bind ((*package* (find-package :keyword)))
    (format nil "~S" symbol)))

(defun class-file-name (class)
  (bind ((class-name (class-name class)))
    (concatenate 'string (fully-qualified-symbol-name class-name) ".html")))

(defun present-class-to-file (&optional (class (find-class 'standard-object)) file-name)
  (with-open-file (*http-stream* (or file-name (concatenate 'string  "/tmp/" (class-file-name class)))
                                 :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (emit (present-class class))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Present on the web

(defun present-class-to-web (path)
  (bind ((class
          (if (search ".html" path)
              (find-class (read-from-string (subseq path 1 (- (length path) (length ".html")))))
              (find-class 'standard-object)))
         (response (ucw:context.response ucw:*context*))
         (*http-stream* (ucw::network-stream response)))
    (setf (ucw::response-managed-p response) nil)
    (ucw::send-headers response)
    (emit (present-class class))))

(defun start-server ()
  (ucw:create-server
   :applications (list
                  (make-instance 'ucw:standard-application
                                 :url-prefix "/"
                                 :dispatchers (list
                                               (ucw:make-simple-dispatcher ""
                                                 (present-class-to-web (ucw::query-path (ucw:context.request ucw:*context*)))))))))
