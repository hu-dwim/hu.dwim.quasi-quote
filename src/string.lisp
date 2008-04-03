;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-string (&key (quasi-quote-character #\[)
                                         (quasi-quote-end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-string-quasi-quote body))))
   (lambda (form spliced)
     (make-string-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-string-to-string ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(string)))

(define-syntax quasi-quoted-string-to-string-emitting-form ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(string-emitting-form)))

(define-syntax quasi-quoted-string-to-binary ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(quasi-quoted-binary binary)))

(define-syntax quasi-quoted-string-to-binary-emitting-form ()
  (set-quasi-quoted-string-syntax-in-readtable :transform '(quasi-quoted-binary binary-emitting-form)))

;;;;;;;
;;; AST
;;;
;;; A quasi quoted string is made of character, string, list, string-quasi-quote, string-unquote nodes recursively.

(def ast string)

(def class* string-syntax-node (syntax-node)
  ())

(def (class* e) string-quasi-quote (quasi-quote string-syntax-node)
  ())

(def (function e) make-string-quasi-quote (body)
  (make-instance 'string-quasi-quote :body body))

(def (class* e) string-unquote (unquote string-syntax-node)
  ())

(def (function e) make-string-unquote (form &optional (spliced? #f))
  (make-instance 'string-unquote :form form :spliced spliced?))

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

(def function write-quasi-quoted-string (node stream)
  (etypecase node
    (character (write-char node stream))
    (string (write-string node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-string node stream)) node))
    (string-quasi-quote (write-quasi-quoted-string (body-of node) stream))
    (function (funcall node)))
  (values))

(def macro with-string-stream-to-string (stream &body forms)
  `(bind ((,stream (make-string-output-stream)))
     ,@forms
     (get-output-stream-string ,stream)))

(def function transform-quasi-quoted-string-to-string-emitting-form (input &key (toplevel #t) (stream '*string-stream*) &allow-other-keys)
  (etypecase input
    (string-quasi-quote
     (labels ((process (node)
                (etypecase node
                  (character `(write-char ,node ,stream))
                  (string
                   (if (= 1 (length node))
                       `(write-char ,(char node 0) ,stream)
                       `(write-string ,node ,stream)))
                  (string-unquote
                   `(write-quasi-quoted-string ,
                     (transform-quasi-quoted-string-to-string-emitting-form node :toplevel #f :stream stream) ,stream))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((forms (reduce-subsequences (flatten (body-of input))
                                          #'stringp
                                          (lambda (&rest elements)
                                            (apply #'concatenate 'string elements))))
              (internal-stream? (eq stream '*string-stream*))
              (processed-forms (if (and toplevel
                                        internal-stream?
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms)))
              (form (if (and toplevel
                             internal-stream?
                             (not (single-string-list-p processed-forms)))
                        `(with-string-stream-to-string ,stream
                           ,@processed-forms)
                        (wrap-forms-with-progn
                         (append processed-forms
                                 (unless (and toplevel
                                              internal-stream?)
                                   `((values))))))))
         (cond ((and toplevel
                     internal-stream?)
                `(make-string-quasi-quote ,form))
               ((or (not internal-stream?)
                    (not toplevel))
                (wrap-forms-with-lambda form nil))
               (t form)))))
    (string-unquote
     (map-tree (form-of input)
               (lambda (form)
                 (if (typep form 'string-quasi-quote)
                     (transform-quasi-quoted-string-to-string-emitting-form form :toplevel #f :stream stream)
                     form))))))

(def method transform ((to (eql 'string-emitting-form)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-string-emitting-form input args))

(def function transform-quasi-quoted-string-to-quasi-quoted-binary (node &rest args &key (encoding :utf-8) &allow-other-keys)
  (etypecase node
    (function node)
    (list
     (mapcar (lambda (child)
               (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary child args))
             node))
    (character (babel:string-to-octets (string node) :encoding encoding)) ;; TODO: more efficient way
    (string (babel:string-to-octets node :encoding encoding))
    (string-quasi-quote
     (make-binary-quasi-quote (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary (body-of node) args)))
    (string-unquote
     (make-binary-unquote
      `(transform-quasi-quoted-string-to-quasi-quoted-binary
        ,(map-filtered-tree (form-of node) 'string-quasi-quote
                            (lambda (child)
                              (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary child args)))
        :encoding ,encoding)))
    (quasi-quote
     (if (typep node 'binary-quasi-quote)
         (body-of node)
         node))
    (unquote (transform 'quasi-quoted-binary node))))

(def method transform ((to (eql 'quasi-quoted-binary)) (input string-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-string-to-quasi-quoted-binary input args))

(def method emit ((node string-syntax-node) &optional stream)
  (bind ((body (call-next-method)))
    (if (and stream
             (typep stream 'string-stream))
        (write-string body stream)
        body)))
