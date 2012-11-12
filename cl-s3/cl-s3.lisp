;;;; -*- Mode: LISP -*-
;;;;
;;;; CL-S3 is a Common Lisp Library for accessing the Amazon S3 Service.
;;;;
;;;; For more information: http://aws.amazon.com/s3
;;;;
;;;; Copyright (C) 2006,2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-s3)

;;; Parameters

(defparameter *amazon-host* "s3.amazonaws.com")

(defvar *access-key-id* ""
  "The value of your Amazon Web Services public access key ID")

(defvar *secret-access-key* ""
  "The value of your Amazon Web Services secret access key")

;;; Support Code

(defun request-date (&optional (universal-time (get-universal-time)))
  "Generate a GMT HTTP Request Date"
  (s-utils:format-universal-time universal-time
                                 :format '(:day-name ", " :date2 #\Space :month-name #\Space :year #\Space 
                                           :hour #\: :minute #\: :second " GMT")
                                 :decode-in-timezone 0))

(defconstant +zero-unix-time+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-time->lisp-time (ms)
  "Convert from Unix time to Common Lisp time"
  (+ +zero-unix-time+ (floor ms 1000)))

(defun lisp-time->unix-time (s)
  "Convert from Common Lisp time to Unix time"
  (* (- s +zero-unix-time+) 1000))

(defun hmac-sha1 (data-string key-string)
  "Compute an RFC 2104 HMAC-SHA1 digest on data-string using key-string"
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key-string) :sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array data-string))
    (ironclad:hmac-digest hmac)))

(defun make-authorization (verb 
                           resource
                           date
                           &key 
                           (secret-access-key *secret-access-key*)
                           content-md5
                           content-type
                           amz-headers)
  "Compute an Amazon S3 authorization signature"
  (let* ((canonical-amz-headers (sort (mapcar #'(lambda (header)
                                                  (destructuring-bind (key . value)
                                                      header
                                                    `(,(string-downcase key) . ,value)))
                                              amz-headers)
                                      #'string-lessp
                                      :key #'car))
         (canonical-string (with-output-to-string (out)
                             (write-string (string-upcase verb) out)
                             (terpri out)
                             (when content-md5 (write-string content-md5 out))
                             (terpri out)
                             (when content-type (write-string content-type out))
                             (terpri out)
                             (when date (write-string date out))
                             (terpri out)
                             (loop :for (key . value) :in canonical-amz-headers :do
                                   (format out "~a:~a~%" key value))
                             (write-string resource out)))
         (hmac-sha1 (hmac-sha1 canonical-string secret-access-key))
         (base64-encoded-result (with-output-to-string (out) 
                                  (s-base64:encode-base64-bytes hmac-sha1 out))))
    (values base64-encoded-result
            canonical-string)))

(defun md5sum-sequence (data-string)
  "Compute a MD5 digest on data-string"
  (ironclad:digest-sequence :md5 (ironclad:ascii-string-to-byte-array data-string)))

#|

;; Rest Authentication Example 1 (s3-dg-20060301.pdf, page 35)

> (make-authorization :put "/quotes/nelson" "Thu, 17 Nov 2005 18:49:58 GMT" :secret-access-key "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV" :content-type "text/html" :content-md5 "c8fdb181845a4ca6b8fec737b3581d76" :amz-headers '((:x-amz-meta-author . "foo@bar.com") (:x-amz-magic . "abracadabra")))

"jZNOcbfWmD/A/f3hSvVzXZjM2HU="

"PUT
c8fdb181845a4ca6b8fec737b3581d76
text/html
Thu, 17 Nov 2005 18:49:58 GMT
x-amz-magic:abracadabra
x-amz-meta-author:foo@bar.com
/quotes/nelson"

;; Rest Authentication Example 2 (s3-dg-20060301.pdf, page 36)

> (make-authorization :get "/quotes/nelson" nil :secret-access-key "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV" :amz-headers '((:x-amz-date . "Thu, 17 Nov 2005 18:49:58 GMT") (:x-amz-magic . "abracadabra")))

"5m+HAmc5JsrgyDelh9+a2dNrzN8="

"GET



x-amz-date:Thu, 17 Nov 2005 18:49:58 GMT
x-amz-magic:abracadabra
/quotes/nelson"

;; Rest Authentication Example 3 (s3-dg-20060301.pdf, page 37)

> (make-authorization :get "/quotes/nelson" "1141889120" :secret-access-key "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV")

"vjbyPxybdZaNmGa+yT272YEAiv4="

"GET


1141889120
/quotes/nelson"

> (s-http-client:uri-encode-for-query *)

"vjbyPxybdZaNmGa%2ByT272YEAiv4%3D"

|#

;;; Predefine the Amazon S3 XML namespace: 

(defparameter *s3-ns*
  (s-xml:register-namespace "http://s3.amazonaws.com/doc/2006-03-01/" "S3" :cl-s3))

;;; An S3 API Condition 

(define-condition amazon-s3-api-error (error)
  ((xml :initarg :xml :reader amazon-s3-api-error-xml)
   (more-values :initarg :more-values))
  (:report (lambda (condition stream)
	     (format stream
		     "Amazon S3 API Error: ~a"
                     (second (find :|Message| (rest (amazon-s3-api-error-xml condition)) 
                                   :key #'(lambda (x) (and (consp x) (first x))))))))
  (:documentation "Thrown when an Amazon S3 API Error is received"))

;;; Internal S3 HTTP request interface

(defun do-s3-request (verb uri &key content content-type content-length content-md5 amz-headers query body)
  (let* ((date (request-date))
         (signature (if content 
                        (make-authorization verb uri date 
                                            :content-type content-type 
                                            :content-md5 content-md5 
                                            :amz-headers amz-headers)
                      (make-authorization verb uri date 
                                          :amz-headers amz-headers)))
         (authorization (format nil "AWS ~a:~a" *access-key-id* signature))
         (query-args (loop :for (name . value) :in query 
                           :when value 
                           :collect (format nil "~a=~a" 
                                            name (s-http-client:uri-encode-for-query (princ-to-string value)))))
         (url (concatenate 'string "http://" *amazon-host* uri))
         (headers `(("Date" . ,date)
                    ("Authorization" . ,authorization)
                    ,@(when content-md5 `(("Content-MD5" . ,(princ-to-string content-md5))))
                    ,@amz-headers)))
    (when query
      (setf url (format nil "~a?~{~a~^&~}" url query-args)))
    (format t ";; CL-S3 ~a ~a~%" verb url)
    (multiple-value-bind (result code headers uri keep-alive-p)
        (if content
            (s-http-client:do-http-request url 
                                           :method verb :headers headers
                                           :content content :content-type content-type :content-length content-length
                                           :body body)
          (s-http-client:do-http-request url :method verb :headers headers :body body))
      (when (and (stringp result) (equal "application/xml" (cdr (assoc :content-type headers))))
        (setf result (s-xml:parse-xml-string result))
        (when (and (< 299 code) (eql (first result) :|Error|))
          (error (make-condition 'amazon-s3-api-error
                                 :xml result 
                                 :more-values (list code headers uri keep-alive-p)))))
      (values result code headers uri keep-alive-p))))

;;; API

(defun get-service ()
  "List all available buckets"
  (do-s3-request :get "/"))

(defun get-bucket (bucket-name &key prefix marker max-keys delimiter)
  "List all keys in a bucket that satisfy a query"
  (do-s3-request :get (concatenate 'string "/" bucket-name)
                 :query `(("prefix" . ,prefix) ("marker" . ,marker) 
                          ("max-keys" . ,max-keys) ("delimiter" . ,delimiter))))

(defun put-bucket (bucket-name)
  "Create a new bucket"
  (do-s3-request :put (concatenate 'string "/" bucket-name)))

(defun delete-bucket (bucket-name)
  "Delete a bucket"
  (do-s3-request :delete (concatenate 'string "/" bucket-name)))

(defun get-object (bucket-name object-key &key check-integrity body)
  "Retrieve the object identified by key in bucket"
  (multiple-value-bind (result code headers uri keep-alive-p)
      (do-s3-request :get (concatenate 'string "/" bucket-name "/" object-key) :body body)
    (if check-integrity
        (let* ((result-md5 (when (stringp result) 
                             (ironclad:byte-array-to-hex-string (md5sum-sequence result))))
               (etag (cdr (assoc :etag headers)))
               (etag-md5 (when etag (read-from-string etag nil))))
          (values result code headers uri keep-alive-p
                  (if (and result-md5 etag-md5 (string-equal result-md5 etag-md5)) :valid :invalid)))
      (values result code headers uri keep-alive-p))))

(defun put-object (bucket-name object-key content content-type &key amz-headers check-integrity content-length)
  "Set the object identified by key in bucket to the specified content"
  (let* ((content-md5 (when check-integrity 
                        (md5sum-sequence content)))
         (content-md5-base64 (when content-md5
                               (with-output-to-string (out) 
                                 (s-base64:encode-base64-bytes content-md5 out))))
         (content-md5-hex (when content-md5
                            (ironclad:byte-array-to-hex-string content-md5))))
    (multiple-value-bind (result code headers uri keep-alive-p)
        (do-s3-request :put (concatenate 'string "/" bucket-name "/" object-key)
                       :content content :content-type content-type :content-length content-length
                       :content-md5 content-md5-base64
                       :amz-headers amz-headers)
      (if check-integrity
        (let* ((etag (cdr (assoc :etag headers)))
               (etag-md5 (when etag (read-from-string etag nil))))
          (values result code headers uri keep-alive-p
                  (if (and content-md5 etag-md5 (string-equal content-md5-hex etag-md5)) :valid :invalid)))
      (values result code headers uri keep-alive-p)))))

(defun head-object (bucket-name object-key)
  "Retrieve all meta-data for the the object identified by key in bucket"
  (do-s3-request :head (concatenate 'string "/" bucket-name "/" object-key)))

(defun delete-object (bucket-name object-key)
  "Delete the object identified by key in bucket"
  (do-s3-request :delete (concatenate 'string "/" bucket-name "/" object-key)))

;;; Some higher level functions that turn Amazon's XML into simpler data
;;; Of course, some useful information is discarded here, so adjust to your own needs!

(defun list-buckets ()
  "Returns a list of all accessible buckets' string names"
  (let* ((data (get-service))
         (buckets (rest (find '|Buckets| data :key #'first))))
    (loop :for bucket :in buckets :collect (second (find '|Name| (rest bucket) :key #'first)))))

(defun list-objects (bucket-name &key prefix marker max-keys delimiter)
  "Return a list of all keys' strings in bucket, satifying the query"
  (flet ((get-tag (lxml) (if (consp lxml) (first lxml) lxml))
         (get-contents (lxml) (if (consp lxml) (second lxml))))
    (let* ((data (get-bucket bucket-name 
                             :prefix prefix :marker marker 
                             :max-keys max-keys :delimiter delimiter))
           (is-truncated (equal (get-contents (find '|IsTruncated| data :key #'get-tag)) "true"))
           (next-marker (get-contents (find '|NextMarker| data :key #'get-tag)))
           (delimiter (get-contents (find '|Delimiter| data :key #'get-tag)))
           (name (get-contents (find '|Name| data :key #'get-tag)))
           (max-keys (parse-integer (get-contents (find '|MaxKeys| data :key #'get-tag))))
           (prefix (get-contents (find '|Prefix| data :key #'get-tag)))
           (keys (if delimiter
                     (loop :for key :in (remove-if-not #'(lambda (x) 
                                                           (and (consp x) (eql (first x) '|CommonPrefixes|))) 
                                                       data)
                           :collect (second (find '|Prefix| (rest key) :key #'first)))
                   (loop :for key :in (remove-if-not #'(lambda (x) 
                                                         (and (consp x) (eql (first x) '|Contents|))) 
                                                     data) 
                         :collect (second (find '|Key| (rest key) :key #'first))))))
      (values keys
              name
              prefix
              marker
              max-keys
              delimiter
              is-truncated
              next-marker))))

;; Example code that makes it easier to copy files to and from S3

(defun download-file (bucket 
                      key 
                      &key 
                      dir (if-exists :supersede) (element-type '(unsigned-byte 8)))
  "Assuming key in bucket denotes a file, download it, optionally in dir"
  (with-open-file (out 
                   (if dir (merge-pathnames key dir) key) 
                   :direction :output :element-type element-type :if-exists if-exists)
    (get-object bucket 
                key 
                :body out)))

(defun upload-file (pathname 
                    bucket 
                    &key 
                    name (element-type '(unsigned-byte 8)) (mime-type "application/octet-stream") acl)
  "Upload the file at pathname under bucket, optionally storing it under name"
  (with-open-file (in 
                   pathname 
                   :direction :input :element-type element-type)
    (put-object bucket 
                (or name (if (pathname-type pathname)
                             (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
                           (pathname-name pathname)))
                in
                mime-type
                :content-length (file-length in)
                ;; allowed acl values are private (default), public-read, public-read-write, authenticated-read
                :amz-headers (when acl `((:x-amz-acl . ,(string-downcase acl)))))))

;;;; eof
