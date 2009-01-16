;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :cl-quasi-quote-js)

(def (js-macro e) |cond| (&rest clauses)
  (if (endp clauses)
      nil
      (let ((clause (first clauses))
            (more (rest clauses)))
        (if (atom clause)
            (error "COND clause is not a list: ~S" clause)
            (let ((test (first clause))
                  (forms (rest clause)))
              (if (endp forms)
                  (with-unique-names (n-result)
                    {with-preserved-readtable-case
                     `(let ((,N-RESULT ,TEST))
                        (if ,N-RESULT
                            ,N-RESULT
                            ,@(WHEN MORE `((cond ,@MORE)))))})
                  (if (member test '(t |t|))
                      {with-preserved-readtable-case
                       `(progn ,@FORMS)}
                      {with-preserved-readtable-case
                       `(if ,TEST
                            (progn ,@FORMS)
                            ,@(WHEN MORE `((cond ,@MORE))))})))))))

(def (js-macro e) |dolist| ((var list) &body body)
  (with-unique-js-names (idx)
    (once-only (list)
      {with-preserved-readtable-case
        `(do ((,IDX 0 (1+ ,IDX)))
             ((>= ,IDX (slot-value ,LIST 'length)))
           (let ((,VAR (aref ,LIST ,IDX)))
             ,@BODY))})))

(def (js-macro e) |rebind| (variables &body body)
  {with-preserved-readtable-case
    `(let ((new-context (create ,@(LOOP
                                     :FOR VARIABLE :IN VARIABLES
                                     :FOR JS-NAME = (LISP-NAME-TO-JS-NAME VARIABLE)
                                     :COLLECT JS-NAME
                                     :COLLECT (MAKE-SYMBOL JS-NAME)))))
       (with new-context
         ,@BODY))})

(macrolet ((frob (name index)
             `(def (js-macro e) ,name (thing)
                `(|aref| ,thing ,,index))))
  (frob |first|   0)
  (frob |second|  1)
  (frob |third|   2)
  (frob |fourth|  3)
  (frob |fifth|   4)
  (frob |sixth|   5)
  (frob |seventh| 6)
  (frob |eight|   7)
  (frob |ninth|   8)
  (frob |tenth|   9))
