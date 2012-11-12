;;;; -*- Mode: LISP -*-
;;;;
;;;; The CL-S3 ASDF system definition
;;;;
;;;; Copyright (C) 2006,2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :cl-s3
  :name "CL-S3"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "1"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "A Common Lisp Amazon S3 client interface package"
  :components ((:file "cl-s3-package")
               (:file "cl-s3" :depends-on ("cl-s3-package")))
  :depends-on (:s-http-client 
               :s-xml
               :s-utils
               :s-base64
               :ironclad))

;;; eof
