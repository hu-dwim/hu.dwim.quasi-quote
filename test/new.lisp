;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

;; TODO: ajax stuff
;; TODO: per session state
;; TODO: quoted AST node state
;; TODO: computed state?

(def function with-typesetting ()
  (with-quasi-quoted-typesetting-to-string-syntax))

;; setup continuation support
(require :cl-cont)

(use-package :cl-cont)

(def special-variable *continuation* nil)

(def macro call (&body forms)
  `(let/cc k
     ,@forms))

(def macro answer ()
  `(setf *continuation* k))

(def macro with-sequential (&body forms)
  `(if *continuation*
       (funcall *continuation*)
       (progn
         ,@forms)))

;; hello world example
(def function hello-world-example ()
  {with-typesetting
      [paragraph "Hello World"]})

;; counter example
(bind ((counter 0))
  (def function counter-example ()
    {with-typesetting
        [vertical-list
         (paragraph "The counter value is now: " ,counter)
         (button "Increment" ,(incf counter))
         (button "Decrement" ,(decf counter))]}))

;; menu example
(bind ((content (lambda () "Please choose a menu item")))
  (def function menu-example ()
    {with-typesetting
        [horizontal-list
         (menu
          (menu-item "Hello World" ,(setf content (lambda () (hello-world-example))))
          (menu-item "Counter" ,(setf content (lambda () (counter-example))))
          (menu-item "Sequential" ,(setf content (lambda () (sequential-example)))))
         ,(funcall content)]}))

;; sequential example
(def function sequential-step (k text)
  {with-typesetting
      [horizontal-list
       ,text
       (button "Next" ,(answer))]})

(defun/cc sequential-example ()
  (with-sequential
    (iter (call (sequential-step k "Step 1"))
          (sequential-sub-example)
          (call (sequential-step k "Step 2")))))

(defun/cc sequential-sub-example ()
  (call (sequential-step k "Step 1.1"))
  (call (sequential-step k "Step 1.2"))
  (call (sequential-step k "Step 1.3")))

;; main screen
(def function main-screen ()
  {with-typesetting
      [screen
       (vertical-list
        ,(hello-world-example)
        ,(counter-example)
        ,(sequential-example)
        ,(menu-example))]})

;; server
(ucw:create-server
 :applications
 (list
  (make-instance 'ucw:standard-application
                 :url-prefix "/"
                 :dispatchers (list (ucw:make-simple-dispatcher ""
                                      (write-string
                                       (bind ((path (ucw::query-path (ucw:context.request ucw:*context*)))
                                              (handler (gethash (subseq path 1) *registered-lambdas*)))
                                         (when handler
                                           (funcall handler))
                                         (main-screen))
                                       (ucw:html-stream (ucw:context.response ucw:*context*))))))))
