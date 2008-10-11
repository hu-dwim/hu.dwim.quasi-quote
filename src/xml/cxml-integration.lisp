;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-xml)

(defparameter +xml-namespace-uri+ "http://www.w3.org/XML/1998/namespace")

(unless (boundp '+whitespace-char-string+)
    (defconstant +whitespace-char-string+
      (coerce
       '(#\Space #\Tab #\Linefeed #\Return #\Page)
       'string)
      "A string of all characters which are considered to be whitespace.
Same as Perl's [\\s]."))

(defclass sax-handler (sax:default-handler)
  ())

(defmethod sax:start-element ((builder sax-handler) namespace-uri local-name qname attributes)
  (locally (declare (special *indent*))
    (format *standard-output* (concatenate 'string "~%~" (format nil "~A" *indent*) "@T<~A") qname )
    (incf *indent*)
    (and (plusp (length attributes)) (format *standard-output* " ("))
    (iter (for attribute :in attributes)
          (if-first-time () (format *standard-output* " "))
          (format *standard-output* "~A \"~A\"" (sax:attribute-qname attribute) (sax:attribute-value attribute)))
    (and (plusp (length attributes)) (format *standard-output* ")"))))


(defmethod sax:end-element ((builder sax-handler) namespace-uri local-name qname)
  (locally (declare (special *indent*))
    (format *standard-output* ">")
    (decf *indent*)))

(defmethod sax:characters ((builder sax-handler) data)
  (bind ((chars (escape-as-xml (string-right-trim +whitespace-char-string+
                                    (string-left-trim +whitespace-char-string+ data)))))
    (and (plusp (length chars)) (format *standard-output* " \"~A\"" chars))))

(def function print-quasi-quoted-xml (input)
  "Emit an xml input as quasi quoted string. Input can be anything acceptable by cxml:parse."
  (flet ((resolver (pubid sysid)
	 (declare (ignore pubid sysid))
	 (flexi-streams:make-in-memory-input-stream nil)))
    (let ((*indent* 0))
      (declare (special *indent*))
      (cxml:parse input (make-instance 'sax-handler) :entity-resolver #'resolver))))


