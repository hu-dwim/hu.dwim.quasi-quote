;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;;;;;;;;
;;; Parse

(define-syntax quasi-quoted-binary (&key (quasi-quote-character #\[)
                                         (quasi-quote-end-character #\])
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (transform nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body)
     (bind ((*quasi-quote-level* (1+ *quasi-quote-level*)))
       (readtime-chain-transform transform (make-binary-quasi-quote body))))
   (lambda (form spliced)
     (make-binary-unquote form spliced))
   :quasi-quote-character quasi-quote-character
   :quasi-quote-end-character quasi-quote-end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-binary-to-binary ()
  (set-quasi-quoted-binary-syntax-in-readtable :transform '(binary)))

(define-syntax quasi-quoted-binary-to-binary-emitting-form ()
  (set-quasi-quoted-binary-syntax-in-readtable :transform '(binary-emitting-form)))

;;;;;;;
;;; AST

(def (type e) binary ()
  '(vector (unsigned-byte 8)))

(def ast binary)

(def class* binary-syntax-node ()
  ())

(def (class* e) binary-quasi-quote (quasi-quote binary-syntax-node)
  ())

(def (function e) make-binary-quasi-quote (body)
  (make-instance 'binary-quasi-quote :body body))

(def (class* e) binary-unquote (unquote binary-syntax-node)
  ())

(def (function e) make-binary-unquote (form &optional (spliced? #f))
  (make-instance 'binary-unquote :form form :spliced spliced?))

;;;;;;;;;;;;;
;;; Transform

(def special-variable *binary-stream*)

(def function binary-position (&optional (stream *binary-stream*))
  (file-position stream))

(def function write-quasi-quoted-binary (node stream)
  (etypecase node
    (vector (write-sequence node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-binary node stream)) node))
    (function (funcall node))
    (binary-quasi-quote (write-quasi-quoted-binary (body-of node) stream)))
  (values))

(def (macro e) with-binary-stream-to-binary (stream &body forms)
  `(bind ((,stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
     ,@forms
     (flexi-streams:get-output-stream-sequence ,stream)))

(def function transform-quasi-quoted-binary-to-binary-emitting-form (input &key (toplevel #t) (stream '*binary-stream*) &allow-other-keys)
  (flet ((process-form (form)
           (cond ((typep form 'quasi-quote)
                  (transform-quasi-quoted-binary-to-binary-emitting-form form :toplevel #f :stream stream))
                 ;; TODO: this is fragile
                 ;; TODO: we would be better of having the stream on the ast returned (even if it is a function)
                 ((and (consp form)
                       (null (cdr form))
                       (eq 'binary-position (car form))
                       (not (eq stream '*binary-stream*)))
                  `(binary-position ,stream))
                 (t form))))
    (etypecase input
      (binary-quasi-quote
       (labels ((process (node)
                  (etypecase node
                    (binary `(write-sequence ,node ,stream))
                    (binary-unquote
                     `(write-quasi-quoted-binary
                       ,(transform-quasi-quoted-binary-to-binary-emitting-form node :toplevel #f :stream stream) ,stream))
                    (side-effect (map-tree (form-of node) #'process-form #t))))
                (single-string-list-p (node)
                  (and (= 1 (length node))
                       (stringp (first node)))))
         (bind ((forms (reduce-subsequences (flatten (body-of input))
                                            #'vectorp
                                            (lambda (&rest elements)
                                              (apply #'concatenate 'binary elements))))
                (internal-stream? (eq stream '*binary-stream*))
                (processed-forms (if (and toplevel
                                          internal-stream?
                                          (single-string-list-p forms))
                                     forms
                                     (mapcar #'process forms)))
                (form (if (and toplevel
                               internal-stream?
                               (not (single-string-list-p processed-forms)))
                          `(with-binary-stream-to-binary ,stream
                             ,@processed-forms)
                          (wrap-forms-with-progn
                           (append processed-forms
                                   (unless (and toplevel
                                                internal-stream?)
                                     `((values))))))))
           (cond ((and toplevel
                       internal-stream?)
                  `(make-binary-quasi-quote ,form))
                 ((or (not internal-stream?)
                      (not toplevel))
                  (wrap-forms-with-lambda form nil))
                 (t form)))))
      (binary-unquote
       (map-tree (form-of input) #'process-form #t)))))

(def method transform ((to (eql 'binary-emitting-form)) (input binary-syntax-node) &rest args &key &allow-other-keys)
  (apply #'transform-quasi-quoted-binary-to-binary-emitting-form input args))

(def method emit ((node binary-syntax-node) &optional stream)
  (bind ((body (call-next-method)))
    (if (and stream
             (not (typep stream 'string-stream)))
        (write-sequence body stream)
        body)))
