;;;; -*- Mode: LISP -*-
;;;;
;;;; The CL-S3 package definition
;;;;
;;;; Copyright (C) 2006,2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :cl-s3
  (:use common-lisp)
  (:export
   #:*access-key-id*
   #:*secret-access-key*
   #:request-date
   #:unix-time->lisp-time
   #:lisp-time->unix-time
   #:make-authorization
   #:amazon-s3-api-error
   #:get-service
   #:get-bucket
   #:put-bucket
   #:delete-bucket
   #:get-object
   #:put-object
   #:head-object
   #:delete-object
   #:list-buckets
   #:list-objects
   #:upload-file
   #:download-file)
  (:documentation "A Common Lisp Amazon S3 client interface package"))

;;; eof
