;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote)

;;; A quasi quoted string is made of string, list, quasi-quote, unquote

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

(def (function e) expand-quasi-quoted-string-to-lambda-form (qq-string &optional (toplevel #t))
  (etypecase qq-string
    (quasi-quote
     (labels ((process (node)
                (etypecase node
                  (string `(write-string ,node *string-stream*))
                  (unquote `(write-quasi-quoted-string (funcall ,(expand-quasi-quoted-string-to-lambda-form node #f))))))
              (single-string-list-p (node)
                (and (= 1 (length node))
                     (stringp (first node)))))
       (bind ((toplevel (or toplevel
                            (toplevel-p qq-string)))
              (forms (reduce-subsequences (flatten (body-of qq-string))
                                          #'stringp
                                          (lambda (&rest elements)
                                            (apply #'concatenate 'string elements))))
              (processed-forms (if (and toplevel
                                        (single-string-list-p forms))
                                   forms
                                   (mapcar #'process forms))))
         (if (and toplevel
                  (not (single-string-list-p processed-forms)))
             `(lambda ()
                (with-quasi-quoted-string-emitting-environment
                  ,@processed-forms))
             `(lambda ()
                ,@processed-forms)))))
    (unquote
     (labels ((process (form)
                (cond ((typep form 'quasi-quote)
                       (if (toplevel-p form)
                           `(funcall ,(expand-quasi-quoted-string-to-lambda-form form (toplevel-p form)))
                           (expand-quasi-quoted-string-to-lambda-form form #f)))
                      ((consp form)
                       (cons (process (car form))
                             (process (cdr form))))
                      (t
                       form))))
       `(lambda ()
          ,(process (form-of qq-string)))))))

(def (function e) transform-quasi-quoted-string-to-quasi-quoted-binary (qq-string)
  (etypecase qq-string
    (quasi-quote
     (labels ((process (node)
                (etypecase node
                  (string (babel:string-to-octets node :encoding :utf-8))
                  (list (mapcar #'process node))
                  (unquote (transform-quasi-quoted-string-to-quasi-quoted-binary node)))))
       (make-instance 'quasi-quote
                      :body (bind ((body (body-of qq-string)))
                              (if (consp body)
                                  (mapcar #'process body)
                                  (list (process body)))))))
    (unquote
     (make-instance 'unquote
                    :form `(babel:string-to-octets ,(form-of qq-string) :encoding :utf-8)))))

(def (function e) transform-quasi-quoted-string-to-string (qq-string)
  (funcall (compile nil (expand-quasi-quoted-string-to-lambda-form qq-string))))
