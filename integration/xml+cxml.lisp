;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.xml)

(def constant +xml-namespace-uri+ "http://www.w3.org/XML/1998/namespace")
(def constant +whitespace-characters+ (coerce
                                        '(#\Space #\Tab #\Linefeed #\Return #\Page)
                                        'simple-base-string)
    "A string of all characters which are considered to be whitespace. Same as Perl's [\\s].")

(defclass sax-handler (sax:default-handler)
  ())

(defmethod sax:start-element ((builder sax-handler) namespace-uri local-name qname attributes)
  (format *standard-output* "~%~A<~A" (make-string-of-spaces (* 2 *xml-indent-level*)) qname)
  (incf *xml-indent-level*)
  (when (plusp (length attributes))
    (format *standard-output* " ("))
  (iter (for attribute :in attributes)
        (unless (first-time-p)
          (write-string " "))
        (format *standard-output* "~A \"~A\"" (sax:attribute-qname attribute) (sax:attribute-value attribute)))
  (when (plusp (length attributes))
    (format *standard-output* ")")))

(defmethod sax:end-element ((builder sax-handler) namespace-uri local-name qname)
  (write-string ">")
  (decf *xml-indent-level*))

(defmethod sax:characters ((builder sax-handler) data)
  (bind ((chars (escape-as-xml (string-trim +whitespace-characters+ data))))
    (when (plusp (length chars))
      (format *standard-output* " \"~A\"" chars))))

(def (function e) print-quasi-quoted-xml (input)
  "Emit an xml input as quasi quoted string. Input can be anything acceptable by cxml:parse."
  (bind ((*xml-indent-level* 0))
    (cxml:parse input (make-instance 'sax-handler)
                :entity-resolver (constantly (make-in-memory-input-stream ""))
                :validate #f)))
