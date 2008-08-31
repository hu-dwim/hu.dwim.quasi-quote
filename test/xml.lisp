;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-quasi-quote-test)

(defsuite* (test/xml :in test))

(def special-variable *xml-stream*)

(def function setup-readtable-for-xml-test (inline? &key (binary? #t) indentation-width)
  (if binary?
      (progn
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*xml-stream*
         :encoding :utf-8
         :with-inline-emitting inline?)
        (enable-quasi-quoted-xml-to-binary-emitting-form-syntax
         '*xml-stream*
         :encoding :utf-8
         :text-node-escaping-method :per-character
         :indentation-width indentation-width
         :with-inline-emitting inline?)
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*xml-stream*
         :encoding :utf-8
         :with-inline-emitting inline?))
      (progn
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*xml-stream*
         :with-inline-emitting inline?)
        (enable-quasi-quoted-xml-to-string-emitting-form-syntax
         '*xml-stream*
         :text-node-escaping-method :per-character
         :indentation-width indentation-width
         :with-inline-emitting inline?)
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*xml-stream*
         :with-inline-emitting inline?))))

(def syntax-test-definer xml-test
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test #f))
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test #t)))

(def syntax-test-definer xml-test/inline
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test #t)))

(def syntax-test-definer xml-test/normal
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test #f)))

(def function read-from-string-with-xml-syntax (string &key indentation-width)
  (with-local-readtable
    (setup-readtable-for-xml-test #t :binary? #f :indentation-width indentation-width)
    (read-from-string string)))

(def function pprint-xml (string &key (indentation-width 2))
  (pprint (macroexpand (read-from-string-with-xml-syntax string :indentation-width indentation-width))))

(def function test-xml-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*xml-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (octets-to-string (funcall (compile nil lambda-form))
                                  :encoding :utf-8)))))

(def function parse-xml-into-sxml (string)
  (labels ((drop-whitespace-nodes (node)
             (etypecase node
               (cons
                (list*
                 (first node)
                 (second node)
                 (iter (for child :in (rest (rest node)))
                       (unless (and (stringp child)
                                    (every 'cl-ppcre::whitespacep child))
                         (collect (drop-whitespace-nodes child))))))
               (string node))))
    (drop-whitespace-nodes (cxml:parse string (cxml-xmls:make-xmls-builder)))))

(def test test/xml/escaping/1 ()
  (is (string= "&lt;1&quot;2&gt;3&lt;&amp;4&gt;"
               (escape-as-xml "<1\"2>3<&4>")))
  (let ((str "alma"))
    (is (eq str (escape-as-xml str)))))

(def xml-test test/xml/escaping/2 ()
  (｢<element attribute="&lt;1&gt;"/>｣
   ｢<element (,@(list (make-xml-attribute "attribute" "<1>")))>｣)
  (｢<element attribute="&lt;1&gt;"/>｣
   ｢<element (attribute "<1>")>｣)
  (｢<element>&lt;tunneled&gt;42&lt;/tunneled&gt;</element>｣
   ｢<element () ,(make-xml-text "<tunneled>42</tunneled>")>｣))

(def xml-test test/xml/simple ()
  (｢<element/>｣
   ｢<element>｣)
  ;; this is braindead here, but let's just test that the name of the xml element is read unconditionally until
  ;; a newline, space, start-character, end-character or the unquote-character.
  (｢<aaa$#@!]{}[]()bbb/>｣
   ｢<aaa$#@!]{}[]()bbb>｣)
  (｢<element attribute="1"/>｣
   ｢<element (:attribute 1)>｣)
  (｢<element attribute1="1" attribute2="2"/>｣
   ｢<element (:attribute1 "1" :attribute2 "2")>｣)
  (｢<element>Hello</element>｣
   ｢<element "Hello">｣)
  ;; test that attribute list is optional
  (｢<element><child/></element>｣
   ｢<element <child>>｣)
  (｢<element><child/></element>｣
   ｢<element () <child>>｣))

(def xml-test test/xml/simple-dispatched ()
  (｢<element/>｣
   ｢`xml(element)｣)
  (｢<element attribute="1"/>｣
   ｢`xml(element (:attribute 1))｣)
  (｢<element attribute1="1" attribute2="2"/>｣
   ｢`xml(element (:attribute1 "1" :attribute2 "2"))｣)
  (｢<element>Hello</element>｣
   ｢`xml(element () "Hello")｣)
  (｢<element><child/></element>｣
   ｢`xml(element () (child))｣)
  (｢<element><child/></element>｣
   ｢`xml(element () child)｣)
  (｢&lt;escaped&gt;｣
   ｢`xml "<escaped>"｣))

(def xml-test test/xml/element-unquoting-dispatched ()
  (｢<element/>｣
   ｢`xml(,"element")｣)
  (｢<element><nested/></element>｣
   ｢`xml(element () ,(make-xml-element "nested"))｣)
  (｢<element><child1/><child2/><child3/><child4 attribute1="1"/><child5/></element>｣
   ｢`xml(element ()
          (child1)
          ,(make-xml-element "child2")
          ,@(list (make-xml-element "child3")
                  (make-xml-element "child4" (list (make-xml-attribute "attribute1" "1"))))
          (child5))｣))

(def xml-test test/xml/element-unquoting ()
  (｢<element/>｣
   ｢<,"element">｣)
  (｢<element><nested/></element>｣
   ｢<element
     ,(make-xml-element "nested")>｣)
  (｢<element><child1/><child2/><child3/><child4 attribute1="1"/><child5/></element>｣
   ｢<element
     <child1>
     ,(make-xml-element "child2")
     ,@(list (make-xml-element "child3")
             (make-xml-element "child4" (list (make-xml-attribute "attribute1" "1"))))
     <child5>>｣)
  (｢<a>foobar</a>｣
   ｢<a "foo" ,"bar">｣))

(def xml-test test/xml/attribute-unquoting ()
  (｢<element attribute="1"/>｣
   ｢<element (,@(list (make-xml-attribute "attribute" "1")))>｣)
  (｢<element attribute=""/>｣
   ｢<element (:attribute ,nil)>｣)
  (｢<element attribute="42"/>｣
   ;; this one is testing that in inline emitting the returned +void+ is not princ-to-string'ed as before.
   ;; the bug was triggered in a <foo (:bar ,`js-inline(42))> situation
   ｢<element (:attribute ,`xml ,42)>｣)
  (｢<element attribute1="1" attribute2="2" attribute3="3" attribute4="4" aTTriUte5="5" attribute6="6"/>｣
   ｢<element (attribute1 1
              ,(make-xml-attribute "attribute2" "2")
              ,@(list (make-xml-attribute "attribute3" "3")
                      (make-xml-attribute "attribute4" "4"))
              aTTriUte5 "5"
              ,(make-xml-attribute "attribute6" "6"))>｣))

(def xml-test test/xml/case-sensitivity ()
   ;; the xml reader is case sensitive, but unquoted regions are returning to the toplevel readtable's readtable-case
  (｢<eLement AttributE1="1"><ElemenT/><fOOO baR="42"/></eLement>｣
   ｢<eLement (AttributE1 1)
    ,@(progn
       (list
        <ElemenT>
        <fOOO (baR 42)>))>｣))

(def xml-test test/xml/nested-through-macro-using-lisp-quasi-quote1 ()
  (｢<taggg attribute="atttr"><foo/></taggg>｣
   ｢(macrolet ((nester (tag-name attribute-value &body body)
                 `<,,tag-name (attribute ,,attribute-value) ,,@body>))
      (nester "taggg" "atttr" <foo>))｣))

(def xml-test test/xml/nested-through-macro-using-lisp-quasi-quote2 ()
  (｢<html><body><foo/><bar/></body></html>｣
   ｢(macrolet ((nester (&body body)
                 ;; first ,@ is for xml, the ,@body is for the lisp quasi-quote
                 `<html <body ,@(list ,@body)>>))
      (nester <foo> <bar>))｣))

(def xml-test test/xml/macrolet-in-unquote ()
  (｢<wrapper><element><tag1>value1</tag1><tag2>value2</tag2></element><last/></wrapper>｣
   ｢(macrolet ((wrapper (&body body)
                 `<wrapper ,@(list ,@body)>))
      (wrapper
       <element
        ,@(macrolet ((x (tag value)
                        `<,,tag () ,,value>))
                    (list
                     (x "tag1" "value1")
                     (x "tag2" "value2")))>
       <last>))｣))

(def test test/xml/sharp-plus-works ()
  (enable-quasi-quoted-xml-syntax)
  (is (eql 42 (read-from-string "#+nil(< 1 2) 42"))))

(def test test/xml/errors ()
  (enable-quasi-quoted-xml-syntax)
  (signals reader-error
    (read-from-string "<element < >>"))
  (signals reader-error
    (read-from-string "<al&ma>"))
  (signals reader-error
    (read-from-string "<alma (kor&te 42)>"))
  (signals end-of-file
    (read-from-string "<element")))

(def test test/xml/less-then-sign-at-toplevel ()
  (enable-quasi-quoted-xml-syntax)
  (is (not (equal '<a> (read-from-string "<a>"))))
  (is (equal '< (read-from-string "<")))
  (is (equal '<= (read-from-string "<=")))
  (is (equal '(< a b) (read-from-string "(< a b)"))))

(def xml-test test/xml/spliced-attribute-list ()
  (｢<element ok="1"/>｣
   ｢<element (,@(when (< 3 4) (list (make-xml-attribute "ok" "1"))))>｣))

(def xml-test/normal test/xml/nested-unquoting ()
  (｢<a><b><c><d/></c></b></a>｣
   ｢<a ,(make-xml-element "b" nil (list <c ,(make-xml-element "d")>))>｣))

(def xml-test test/xml/mixed ()
  (｢<element>Hello UN<>QUOTED World</element>｣
   ｢<element `str("Hello" ,(list " UN<>QUOTED " "World"))>｣)
  (｢<element attribute="42"/>｣
   ｢<element (:attribute `str("4" ,(list "2")))>｣)
  (｢<element attribute="fooBAR"/>｣
   ｢<element (:attribute `str("foo" ,(string-upcase "bar")))>｣))

(def xml-test/normal test/xml/reverse ()
  ("<element><child2/><child1/></element>"
   ｢<element ,@(let ((c1 <child1>)
                    (c2 <child2>))
                   (list c2 c1))>｣))
