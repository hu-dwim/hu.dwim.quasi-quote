(cl:in-package :cl-user)

(defpackage :hu.dwim.zlib
  (:documentation "A CFFI based zlib")

  (:use :common-lisp)

  (:export
   #:compress
   #:allocate-compress-buffer
   #:uncompress
   #:deflate
   #:inflate
   ))

(in-package :hu.dwim.zlib)

(defconstant +default-buffer-size+ 4096)

(defconstant +z-no-flush+   0)
(defconstant +z-sync-flush+ 2)
(defconstant +z-full-flush+ 3)
(defconstant +z-finish+     4)

(defconstant +z-ok+            0)
(defconstant +z-stream-end+    1)
(defconstant +z-need-dict+     2)
(defconstant +z-errno+         -1)
(defconstant +z-stream-error+  -2)
(defconstant +z-data-error+    -3)
(defconstant +z-mem-error+     -4)
(defconstant +z-buf-error+     -5)
(defconstant +z-version-error+ -6)

(defconstant +z-no-compression+      0)
(defconstant +z-best-speed+          1)
(defconstant +z-best-compression+    9)
(defconstant +z-default-compression+ -1)

(defconstant +z-deflated+ 8)

(defconstant +z-filtered+         1)
(defconstant +z-huffman-only+     2)
(defconstant +z-default-strategy+ 0)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *zlib-search-paths*
    `(,(directory-namestring (or *load-truename* (truename "./")))
       #+lispworks
       ,(directory-namestring (lw:lisp-image-name))
       "/usr/local/lib/"
       "/usr/lib/"
       "/windows/system32/"
       "/winnt/system32/")
    "The paths where to search the zlib shared library")

  (cffi:define-foreign-library zlib
    (:unix "libz.so")
    (:windows "libz.dll")
    (t (:default "libz")))

  (let ((cffi:*foreign-library-directories* *zlib-search-paths*))
    (cffi:load-foreign-library 'zlib)))

;;;
;;; CFFI definitions
;;;
(cffi:defcfun (%compress2 "compress2" :library zlib) :int
  (dest (:pointer :unsigned-char))
  (destlen (:pointer :long))
  (source (:pointer :unsigned-char))
  (sourcelen :long)
  (level :int))

(cffi:defcfun (%uncompress "uncompress" :library zlib) :int
  (dest (:pointer :unsigned-char))
  (destlen (:pointer :long))
  (source (:pointer :unsigned-char))
  (sourcelen :long))

(cffi:defcstruct z-stream
  (next-in    (:pointer :unsigned-char))
  (avail-in   :unsigned-int)
  (total-in   :unsigned-long)

  (next-out   (:pointer :unsigned-char))
  (avail-out  :unsigned-int)
  (total-out  :unsigned-long)

  (msg        :string)

  (internal-state :pointer)
  (zalloc     :pointer)
  (zfree      :pointer)

  (opaque     :pointer)
  (data-type  :int)
  (adler      :unsigned-long)
  (reserved   :unsigned-long))

(cffi:defcfun (%zlib-version "zlibVersion" :library zlib) :pointer)

(cffi:defcfun (%deflate-init-2 "deflateInit2_" :library zlib) :int
  (stream (:pointer z-stream))
  (level :int)
  (method :int)
  (window-bits :int)
  (mem-level :int)
  (strategy :int)
  (version (:pointer :char))
  (stream-struct-size :int))

(cffi:defcfun (%deflate "deflate" :library zlib) :int
  (stream (:pointer z-stream))
  (flush :int))

(cffi:defcfun (%deflate-end "deflateEnd" :library zlib) :int
  (stream (:pointer z-stream)))

(cffi:defcfun (%inflate-init-2 "inflateInit2_" :library zlib) :int
  (stream (:pointer z-stream))
  (window-bits :int)
  (version (:pointer :char))
  (stream-struct-size :int))

(cffi:defcfun (%inflate "inflate" :library zlib) :int
  (stream (:pointer z-stream))
  (flush :int))

(cffi:defcfun (%inflate-end "inflateEnd" :library zlib) :int
  (stream (:pointer z-stream)))

(assert (= (cffi:foreign-type-size 'z-stream) 112) () "Hm, something's wrong with the length of the z-stream CFFI struct?!")

;;;
;;; Lisp part
;;;
(defun allocate-compress-buffer (source &key (source-start 0) (source-end (length source)))
  (cffi:make-shareable-byte-vector (ceiling (* (+ (- source-end source-start) 12) 1.01))))

(defun zlib-error (code)
  (error "zlib error, code ~D" code))

(defmacro zlib-call (expression &body body)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,expression))
       (if (minusp ,result)
           (zlib-error ,result)
           (progn
             ,@body
             ,result)))))

(defun compress (source destination &key
                 (source-start 0)
                 (source-end (length source))
                 (destination-start 0)
                 (level +z-default-compression+))
  "Compress the first SOURCE-START bytes of SOURCE into DESTINATION. DESTINATION, when not null, should be an array of (unsigned-byte 8), and should be large enough to hold the compressed contents. Returns (values destination destination-end).

Note that the size of the DESTINATION array should be at least 0.1% more than the souce plus 12 bytes, but the actual number of array elements filled in by the compression algorithm will usually be smaller (depending on how 'predictable' the input data is)."
  (declare (type (simple-array (unsigned-byte 8) (*)) source)
           (type (or null (simple-array (unsigned-byte 8) (*))) destination))
  (cffi:with-foreign-object (compressed-length :long)
    (setf (cffi:mem-ref compressed-length :long) (- (length destination) destination-start))
    (cffi:with-pointer-to-vector-data (source-bytes source)
      (cffi:with-pointer-to-vector-data (destination-bytes destination)
        (zlib-call (%compress2 (cffi:inc-pointer destination-bytes destination-start)
                               compressed-length
                               (cffi:inc-pointer source-bytes source-start)
                               (- source-end source-start)
                               level)
          (let ((destination-end (+ destination-start (cffi:mem-ref compressed-length :long))))
            destination-end))))))

(defun uncompress (source destination &key
                   (source-start 0)
                   (source-end (length source))
                   (destination-start 0))
  "DESTINATION must be long enough to hold the uncompressed contents, otherwise errors out. Returns (values DESTINATION destination-end)."
  (declare (type (simple-array (unsigned-byte 8) (*)) source)
           (type (simple-array (unsigned-byte 8) (*)) destination))
  (cffi:with-foreign-object (uncompressed-length :long)
    (setf (cffi:mem-ref uncompressed-length :long) (- (length destination) destination-start))
    (cffi:with-pointer-to-vector-data (source-bytes source)
      (cffi:with-pointer-to-vector-data (destination-bytes destination)
        (zlib-call (%uncompress (cffi:inc-pointer destination-bytes destination-start)
                                uncompressed-length
                                (cffi:inc-pointer source-bytes source-start)
                                (- source-end source-start))
          (let ((destination-end (+ destination-start (cffi:mem-ref uncompressed-length :long))))
            destination-end))))))

(defun make-deflate-z-stream (&key (level +z-default-compression+) (method +z-deflated+)
                      (window-bits 15) (memory-level 8) (strategy +z-default-strategy+))
  (let ((stream (cffi:foreign-alloc 'z-stream))
        (ok nil))
    (unwind-protect
         (progn
           (setf (cffi:foreign-slot-value stream 'z-stream 'zalloc) (cffi:null-pointer))
           (setf (cffi:foreign-slot-value stream 'z-stream 'zfree)  (cffi:null-pointer))
           (setf (cffi:foreign-slot-value stream 'z-stream 'opaque) (cffi:null-pointer))
           (setf (cffi:foreign-slot-value stream 'z-stream 'avail-in) 0)
           (setf (cffi:foreign-slot-value stream 'z-stream 'avail-out) 0)
           (zlib-call (%deflate-init-2 stream level method window-bits memory-level strategy
                                       (%zlib-version) (cffi:foreign-type-size 'z-stream)))
           (setf ok t)
           stream)
      (unless ok
        (cffi:foreign-free stream)))))

(defun free-deflate-z-stream (stream)
  (zlib-call (%deflate-end stream))
  (cffi:foreign-free stream)
  (values))

(defun deflate (input-fn output-fn &key (buffer-size +default-buffer-size+) (level +z-default-compression+)
                (method +z-deflated+) (window-bits 15) (memory-level 8) (strategy +z-default-strategy+))
  (%inflate-or-deflate
   :deflate
   (lambda ()
     (make-deflate-z-stream :level level :method method :window-bits window-bits
                            :memory-level memory-level :strategy strategy))
   'free-deflate-z-stream
   input-fn
   output-fn
   :buffer-size buffer-size))

(defun make-inflate-z-stream (&key (window-bits 15))
  (let ((stream (cffi:foreign-alloc 'z-stream))
        (ok nil))
    (unwind-protect
         (progn
           (setf (cffi:foreign-slot-value stream 'z-stream 'zalloc) (cffi:null-pointer))
           (setf (cffi:foreign-slot-value stream 'z-stream 'zfree)  (cffi:null-pointer))
           (setf (cffi:foreign-slot-value stream 'z-stream 'opaque) (cffi:null-pointer))
           (setf (cffi:foreign-slot-value stream 'z-stream 'avail-in) 0)
           (setf (cffi:foreign-slot-value stream 'z-stream 'avail-out) 0)
           (zlib-call (%inflate-init-2 stream window-bits
                                       (%zlib-version) (cffi:foreign-type-size 'z-stream)))
           (setf ok t)
           stream)
      (unless ok
        (cffi:foreign-free stream)))))

(defun free-inflate-z-stream (stream)
  (zlib-call (%inflate-end stream))
  (cffi:foreign-free stream)
  (values))

(defun inflate (input-fn output-fn &key (buffer-size +default-buffer-size+) (window-bits 15))
  (%inflate-or-deflate
   :inflate
   (lambda ()
     (make-inflate-z-stream :window-bits window-bits))
   'free-inflate-z-stream
   input-fn
   output-fn
   :buffer-size buffer-size))

(defun %inflate-or-deflate (operation make-stream-fn free-stream-fn input-fn output-fn &key
                            (buffer-size +default-buffer-size+))
  (declare (type (member :deflate :inflate) operation))
  (assert (> buffer-size 256))
  (let* ((input-buffer-size (floor buffer-size 2))
         (output-buffer-size (floor buffer-size 2))
         (stream (funcall make-stream-fn))
         (lisp-input-buffer (cffi:make-shareable-byte-vector input-buffer-size))
         (lisp-output-buffer (cffi:make-shareable-byte-vector output-buffer-size)))
    (macrolet ((debug (message &rest args)
                 `(progn
                    (format *debug-io* ,message ,@args)
                    (terpri *debug-io*))
                 nil))
      (unwind-protect
           (cffi:with-pointer-to-vector-data (input-buffer lisp-input-buffer)
             (cffi:with-pointer-to-vector-data (output-buffer lisp-output-buffer)
               (labels ((update-input-if-needed ()
                          (let ((avail-in (cffi:foreign-slot-value stream 'z-stream 'avail-in)))
                            (if (zerop avail-in)
                                (let ((new-input-bytes (funcall input-fn lisp-input-buffer 0 input-buffer-size)))
                                  (if (and new-input-bytes
                                           (plusp new-input-bytes))
                                      (progn
                                        (setf (cffi:foreign-slot-value stream 'z-stream 'next-in) input-buffer)
                                        (setf (cffi:foreign-slot-value stream 'z-stream 'avail-in) new-input-bytes)
                                        (debug "got ~A input bytes" new-input-bytes)
                                        (= new-input-bytes input-buffer-size))
                                      nil))
                                (progn
                                  (debug "input-buffer still has ~A bytes" avail-in)
                                  t))))
                        (reset-output-buffer ()
                          (debug "resetting output buffer")
                          (setf (cffi:foreign-slot-value stream 'z-stream 'next-out) output-buffer)
                          (setf (cffi:foreign-slot-value stream 'z-stream 'avail-out) output-buffer-size))
                        (update-output ()
                          (let* ((avail-out (cffi:foreign-slot-value stream 'z-stream 'avail-out))
                                 (bytes (- output-buffer-size avail-out)))
                            (debug "outputting ~A bytes while avail-out is ~A and output-buffer-size is ~A" bytes avail-out output-buffer-size)
                            (unless (zerop bytes)
                              (funcall output-fn lisp-output-buffer 0 bytes)
                              (reset-output-buffer)))))
                 (reset-output-buffer)
                 ;; this is some idiotic code mimiced from http://www.zlib.net/zlib_how.html
                 (loop
                    :with has-more-input? = t
                    :while has-more-input?
                    :do (progn
                          (setf has-more-input? (update-input-if-needed))
                          (let ((flush (if has-more-input?
                                           +z-no-flush+
                                           +z-finish+)))
                            (loop :named inner :do
                               (progn
                                 (debug "entering inner loop with flush: ~A, avail-in: ~A, avail-out: ~A" flush (cffi:foreign-slot-value stream 'z-stream 'avail-in) (cffi:foreign-slot-value stream 'z-stream 'avail-out))
                                 (ecase operation
                                   (:deflate (zlib-call (%deflate stream flush)))
                                   (:inflate (zlib-call (%inflate stream flush))))
                                 (debug "avail-in is ~A, avail-out is ~A" (cffi:foreign-slot-value stream 'z-stream 'avail-in) (cffi:foreign-slot-value stream 'z-stream 'avail-out))
                                 (let ((output-buffer-full? (zerop (cffi:foreign-slot-value stream 'z-stream 'avail-out))))
                                   (debug "output-buffer-full? ~A" output-buffer-full?)
                                   (update-output)
                                   (unless output-buffer-full?
                                     (return-from inner)))))))))))
        (funcall free-stream-fn stream)))))

#|

(with-open-file (input "/tmp/x.diff" :direction :input :element-type '(unsigned-byte 8))
  (with-open-file (output "/tmp/x.diff.z" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (hu.dwim.zlib:deflate
        (lambda (buffer start size)
          (read-sequence buffer input :start start :end size))
        (lambda (buffer start size)
          (write-sequence buffer output :start start :end size)))))

(with-open-file (input "/tmp/x.diff.z" :direction :input :element-type '(unsigned-byte 8))
  (with-open-file (output "/tmp/x.diff.extracted" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (hu.dwim.zlib:inflate
     (lambda (buffer start size)
       (read-sequence buffer input :start start :end size))
     (lambda (buffer start size)
       (write-sequence buffer output :start start :end size)))))

|#
