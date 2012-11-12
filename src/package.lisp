;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is the S-REST package definition
;;;;
;;;; Copyright (C) 2002-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :s-rest
  (:use common-lisp cl-who)
  (:export
   #:define-rest-resource 
   #:get-rest-resource-handler 
   #:describe-rest-resource
   #:generate-html-for-rest-resources
   #:get-rest-uri
   #:define-s-expr-representation
   #:hash-table->s-expr
   #:object->s-expr
   #:s-expr->hash-table
   #:s-expr->object
   #:universal-time->iso-8601-gmt
   #:iso-8601-gmt->universal-time
   #:encode-string
   #:decode-string
   #:print-s-expr
   #:read-s-expr
   #:s-expr-getf
   #:s-expr-setf
   #:s-expr-replace
   #:s-expr-remf
   #:s-expr-select
   #:s-expr-get-keys
   #:s-expr-get-values
   #:make-s-expr-sorter
   #:make-s-expr-key-equals-value
   #:map-s-expr
   #:rest-get
   #:rest-put
   #:rest-post
   #:rest-delete
   #:resource-not-found
   #:build-rest-call-uri
   #:do-rest-call
   #:*default-host*
   #:with-host
   #:do-http-request
   #:do-get
   #:do-post
   #:do-put
   #:do-delete
   #:rest-exception
   #:resource-not-found
   #:rest-exceptions
   #:get-message
   #:get-code
   #:get-arguments
   #:get-rest-exceptions
   #:get-query
   #:get-headers
   #:get-pattern
   #:to-s-expr
   #:from-s-expr
   #:make-s-http-rest-server
   #:*debug-stream*
   #:*server-debug-stream*
   #:*default-http-client-state*
   #:+s-expr-mime-type+)
  (:documentation "A Common Lisp REST Framework"))

;;;; eof
