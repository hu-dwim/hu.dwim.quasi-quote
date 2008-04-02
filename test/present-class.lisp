(in-package :cl-quasi-quote)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Turn on special syntax

(enable-quasi-quoted-xml-syntax
 :transform '(quasi-quoted-string
              (quasi-quoted-binary :encoding :utf-8)
              (binary-emitting-form :stream *http-stream*)))

(defvar *http-stream*)

;;;;;;;;;;;;;;;;;;;;;;;
;;; The class inspector

(defun present-class (class)
  (let ((class-name (symbol-name (class-name class))))
    <html
     <head
      <title ,class-name>>
     <body
      <h2 "The class " ,class-name " is an instance of " ,(symbol-name (class-name (class-of class)))>
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

(defun present-class-reference (class)
  <a (:href ,(arnesi:escape-as-uri (class-file-name class))) ,(symbol-name (class-name class))>)

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
      <table
          <thead (:bgcolor "#888888")
                 <td <i "Name">>
                 <td <i "Type">>
                 <td <i "Readers">>
                 <td <i "Writers">>>
        ,@(loop
             for index :from 0
             for slot :in slots
             collect (present-slot slot index))>
      <span "There are none">))

(defun present-slot (slot index)
  <tr (:bgcolor ,(if (oddp index) "#eeffbb" "#ffffff"))
      <td <b ,(symbol-name (slot-definition-name slot))>>
      <td ,(princ-to-string (slot-definition-type slot))>
      <td ,(princ-to-string (slot-definition-readers slot))>
      <td ,(princ-to-string (slot-definition-writers slot))>>)

;;;;;;;;;;;;;;;;;;;;;
;;; Present in a file

(defun class-file-name (class)
  (bind ((class-name (class-name class)))
    (concatenate 'string (package-name (symbol-package class-name)) "::" (symbol-name class-name) ".html")))

(defun present-class-to-file (&optional (class (find-class 'standard-object)) file-name)
  (with-open-file (*http-stream* (or file-name (concatenate 'string  "/tmp/" (class-file-name class)))
                                 :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (emit (present-class class) *http-stream*)))

;;;;;;;;;;;;;;;;;;;;;;
;;; Present on the web

(defun present-class-to-web (path)
  (bind ((class
          (if (search ".html" path)
              (bind ((name (subseq path 1 (- (length path) (length ".html"))))
                     ((package-name nil symbol-name) (split-sequence:split-sequence #\: name)))
                (find-class (find-symbol symbol-name package-name)))
              (find-class 'standard-object)))
         (*http-stream*
          (ucw::network-stream (ucw:context.response ucw:*context*))))
    (emit (present-class class) *http-stream*)))

(defun start-server ()
  (ucw:create-server
   :applications (list
                  (make-instance 'ucw:standard-application
                                 :url-prefix "/"
                                 :dispatchers (list
                                               (ucw:make-simple-dispatcher ""
                                                 (present-class-to-web (ucw::query-path (ucw:context.request ucw:*context*)))))))))
