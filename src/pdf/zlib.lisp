(cl:in-package :cl-user)

(defpackage :hu.dwim.zlib
  (:documentation "A CFFI based zlib")

  (:use :common-lisp)

  (:export
   #:compress
   #:allocate-compress-buffer
   #:uncompress
   ))

(in-package :hu.dwim.zlib)

(defconstant +z-ok+             0)
(defconstant +z-errno+         -1)
(defconstant +z-stream-error+  -2)
(defconstant +z-data-error+    -3)
(defconstant +z-mem-error+     -4)
(defconstant +z-buf-error+     -5)
(defconstant +z-version-error+ -6)

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

(cffi:defcfun (%compress "compress" :library zlib) :int
  (dest (:pointer :unsigned-char))
  (destlen (:pointer :long))
  (source (:pointer :unsigned-char))
  (sourcelen :long))

(cffi:defcfun (%uncompress "uncompress" :library zlib) :int
  (dest (:pointer :unsigned-char))
  (destlen (:pointer :long))
  (source (:pointer :unsigned-char))
  (sourcelen :long))

(defun allocate-compress-buffer (source &key (source-start 0) (source-end (length source)))
  (cffi:make-shareable-byte-vector (ceiling (* (+ (- source-end source-start) 12) 1.01))))

(defun compress (source destination &key (source-start 0) (source-end (length source)) (destination-start 0))
  "Compress the first SOURCE-START bytes of SOURCE into DESTINATION. DESTINATION, when not null, should be an array of (unsigned-byte 8), and should be large enough to hold the compressed contents. Returns (values destination destination-end).

Note that the size of the DESTINATION array should be at least 0.1% more than the souce plus 12 bytes, but the actual number of array elements filled in by the compression algorithm will usually be smaller (depending on how 'predictable' the input data is)."
  (declare (type (simple-array (unsigned-byte 8) (*)) source)
           (type (or null (simple-array (unsigned-byte 8) (*))) destination))
  (cffi:with-foreign-object (compressed-length :long)
    (setf (cffi:mem-ref compressed-length :long) (- (length destination) destination-start))
    (cffi:with-pointer-to-vector-data (source-bytes source)
      (cffi:with-pointer-to-vector-data (destination-bytes destination)
        (let ((result (%compress (cffi:inc-pointer destination-bytes destination-start)
                                 compressed-length
                                 (cffi:inc-pointer source-bytes source-start)
                                 (- source-end source-start))))
          (if (zerop result)
              (let ((destination-end (+ destination-start (cffi:mem-ref compressed-length :long))))
                destination-end)
              (error "zlib error, code ~D" result)))))))

(defun uncompress (source destination &key (source-start 0) (source-end (length source)) (destination-start 0))
  "DESTINATION must be long enough to hold the uncompressed contents, otherwise errors out. Returns (values DESTINATION destination-end)."
  (declare (type (simple-array (unsigned-byte 8) (*)) source)
           (type (simple-array (unsigned-byte 8) (*)) destination))
  (cffi:with-foreign-object (uncompressed-length :long)
    (setf (cffi:mem-ref uncompressed-length :long) (- (length destination) destination-start))
    (cffi:with-pointer-to-vector-data (source-bytes source)
      (cffi:with-pointer-to-vector-data (destination-bytes destination)
        (let ((result (%uncompress (cffi:inc-pointer destination-bytes destination-start)
                                   uncompressed-length
                                   (cffi:inc-pointer source-bytes source-start)
                                   (- source-end source-start))))
          (if (zerop result)
              (let ((destination-end (+ destination-start (cffi:mem-ref uncompressed-length :long))))
                destination-end)
              (error "zlib error, code ~d" result)))))))
