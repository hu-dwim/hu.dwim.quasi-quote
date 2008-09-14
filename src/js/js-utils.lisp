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
                    {(with-readtable-case :preserve)
                     `(let ((,N-RESULT ,TEST))
                        (if ,N-RESULT
                            ,N-RESULT
                            ,@(WHEN MORE `((cond ,@MORE)))))})
                  (if (member test '(t |t|))
                      {(with-readtable-case :preserve)
                       `(progn ,@FORMS)}
                      {(with-readtable-case :preserve)
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
