;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: s-xml-rpc.asd,v 1.2 2004/06/17 19:43:11 rschlatte Exp $
;;;;
;;;; The S-REST ASDF system definition
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-rest
  :name "S-REST"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "1"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "A Common Lisp REST Package"
  :long-description "A Common Lisp REST Package"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "encoding" :depends-on ("package"))
                 (:file "rest" :depends-on ("package" "encoding"))
                 (:file "documentation" :depends-on ("package" "rest")))))
  
  :depends-on (:s-http-client :s-http-server :cl-who))

;;;; eof
