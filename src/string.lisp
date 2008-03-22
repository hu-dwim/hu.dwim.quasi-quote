;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-string (&key (quasi-quote-character #\[)
                                         (quasi-quote-end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body) (make-instance 'string-quasi-quote :body body))
   (lambda (form spliced) (make-instance 'string-unquote :form form :spliced spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))


(def (function e) with-quasi-quoted-string-to-binary-syntax ()
  (with-transformed-quasi-quoted-syntax 'quasi-quoted-string 'quasi-quoted-binary 'binary-emitting-form))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted string is made of string, list, string-quasi-quote, string-unquote recursively

(def ast string)

(def class* string-syntax-node (syntax-node)
  ())

(def (class* e) string-quasi-quote (quasi-quote string-syntax-node)
  ())

(def (class* e) string-unquote (unquote string-syntax-node)
  ())

;;;;;;;;;;;;;
;;; Transform

(def special-variable *string-stream*)

(def function reduce-subsequences (sequence predicate reducer)
  (iter (with completely-reduced? = #t)
        (with length = (length sequence))
        (for index :from 0 :below length)
        (for reducibles = (iter (while (< index length))
                                (for element = (elt sequence index))
                                (while (funcall predicate element))
                                (collect element)
                                (incf index)))
        (collect (if (zerop (length reducibles))
                     (progn
                       (setf completely-reduced? #f)
                       (elt sequence index))
                     (progn
                       (decf index)
                       (apply reducer reducibles)))
          :into result)
        (finally (return (values result completely-reduced?)))))

(def function write-quasi-quoted-string (node)
  (etypecase node
    (string (write-string node *string-stream*))
    (list (mapc #'write-quasi-quoted-string node))
    (function (funcall node)))
  (values))

(def macro with-quasi-quoted-string-emitting-environment (&body forms)
  `(bind ((*string-stream* (make-string-output-stream)))
     ,@forms
     (get-output-stream-string *string-stream*)))

(def (macro e) force-quasi-quoted-string (node)
  `(with-quasi-quoted-string-emitting-environment
     (write-quasi-quoted-string ,node)))

(def method transform ((to (eql 'string)) (input string-syntax-node) &key args  &allow-other-keys)
  (funcall (apply #'transform 'string-emitting-lambda input args)))

(def method transform ((to (eql 'string-emitting-form)) (input string-syntax-node) &key (toplevel #t) (stream '*string-stream*) &allow-other-keys)
  (etypecase input
    (string-quasi-quote
     (labels ((process (node)
                (etypecase node
                  (string `(write-string ,node ,stream))
                  (string-unquote `(write-quasi-quoted-string ,(transform 'string-emitting-form node :toplevel #f)))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((forms (reduce-subsequences (flatten (body-of input))
                                          #'stringp
                                          (lambda (&rest elements)
                                            (apply #'concatenate 'string elements))))
              (processed-forms (if (and toplevel
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms))))
         (if (and toplevel
                  (not (single-string-list-p processed-forms)))
             `(with-quasi-quoted-string-emitting-environment
                ,@processed-forms)
             `(progn
                ,@processed-forms)))))
    (string-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'string-quasi-quote)
                     (transform 'string-emitting-lambda-form form :toplevel #f)
                     form))))))

(def method transform ((to (eql 'binary)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform 'binary (transform 'quasi-quoted-binary input) args))

(def function transform-quasi-quoted-string-to-quasi-quoted-binary (node &key (encoding :utf-8) &allow-other-keys)
  (etypecase node
    (function node)
    (binary-quasi-quote node)
    (binary-unquote node)
    (list (mapcar #'transform-quasi-quoted-string-to-quasi-quoted-binary node))
    (string (babel:string-to-octets node :encoding encoding))
    (string-quasi-quote
     (make-instance 'binary-quasi-quote :body (transform-quasi-quoted-string-to-quasi-quoted-binary (body-of node))))
    (string-unquote
     (make-instance 'binary-unquote
                    :form `(map-tree
                            ,(map-filtered-tree (form-of node) 'string-quasi-quote #'transform-quasi-quoted-string-to-quasi-quoted-binary)
                            'transform-quasi-quoted-string-to-quasi-quoted-binary)))))

(def method transform ((to (eql 'quasi-quoted-binary)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary input args))
