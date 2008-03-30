;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-binary-to-binary-emitting-form-syntax)

(defsuite* (test/binary :in test))

(def binary=-test test/binary/simple ()
  (#(1 2)
    [#(1 2)])
  
  (#(1 2 3 4)
    [#(1 2)
     #(3 4)])
  
  (#(1 2 3 4 5 6 7 8)
    [#(1 2)
      (#(3 4)
       #(5 6))
     #(7 8)]))

(def binary=-test test/binary/unquote ()
  (#(1 2 3 4 5 6)
    [#(1 2)
     ,#(3 4)
     #(5 6)])

  (#(1 2 3 4 5 6 7 8 9 10 11 12)
    [#(1 2)
     ,(list
       #(3 4)
       [#(5 6) #(7 8)]
       #(9 10))
     #(11 12)])

  (#(1 2 3 4 5 6 7 8 9 10 11 12)
    [#(1 2)
     ,(concatenate 'vector
                   #(3 4)
                   (force-quasi-quoted-binary [#(5 6) #(7 8)])
                   #(9 10))
     #(11 12)]))

(def binary=-test test/binary/spliced-unquote ()
  (#(1 2 3 4 5 6 7)
    [#(1)
     ,(make-array 1 :initial-element 2)
     ,@(list #(3) #(4) #(5))
     ,(make-array 1 :initial-element 6)
     #(7)]))
