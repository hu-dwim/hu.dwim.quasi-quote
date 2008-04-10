;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(enable-quasi-quoted-binary-syntax)

(defsuite* (test/binary :in test))

(def test-definer binary)

(def special-variable *binary-stream*)

(def function test-binary-ast (expected ast)
  ;; evaluate to binary
  (is (equalp expected
              (transform-and-emit '(binary-emitting-form
                                    lambda-form
                                    lambda)
                                  ast)))
  ;; write to binary stream
  (is (equalp expected
              (bind ((*binary-stream* (make-in-memory-output-stream)))
                (transform-and-emit '((binary-emitting-form :stream-name *binary-stream*)
                                      lambda-form
                                      lambda)
                                    ast)
                (get-output-stream-sequence *binary-stream*)))))

(def binary-test test/binary/simple ()
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

(def binary-test test/binary/unquote ()
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

  (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    [#(1 2)
     ,(list
       #(3 4)
       [#(5 6)
        ,(list #(7 8) #(9 10))
        #(11 12)]
       #(13 14))
     #(15 16)]))

(def binary-test test/binary/spliced-unquote ()
  (#(1 2 3 4 5 6 7)
    [#(1)
     ,(make-array 1 :initial-element 2)
     ,@(list #(3) #(4) #(5))
     ,(make-array 1 :initial-element 6)
     #(7)]))

(def binary-test test/binary/reverse ()
  (#(1 2 3 4 5 6 7 8)
   [#(1 2)
    ,(reverse
      (list [#(5 6)] [#(3 4)]))
    #(7 8)]))
