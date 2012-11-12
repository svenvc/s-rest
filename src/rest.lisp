;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is a Common Lisp REST Framework
;;;;
;;;; Copyright (C) 2005,2006,2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-rest)

;; global structures modeling REST

(defvar *debug-stream* nil)
(defvar *server-debug-stream* nil)

(defvar *rest-resource-handlers* '())

(defclass rest-resource-handler ()
  ((name :accessor get-name :initarg :name)
   (pattern :accessor get-pattern :initarg :pattern :initform nil)
   (variables :accessor get-variables :initarg :variables :initform nil)
   (methods :accessor get-methods :initarg :methods :initform nil)
   (documentation :accessor get-documentation :initarg :documentation :initform nil)))

(defun ensure-rest-resource-handler (handler)
  (if (member (get-name handler) *rest-resource-handlers* :key #'get-name)
      (setf *rest-resource-handlers* (delete (get-name handler) *rest-resource-handlers* :key #'get-name)))
  (push handler *rest-resource-handlers*))

(defmacro define-rest-resource (name pattern docstring &body rest)
  (let* ((question-mark-position (position :? pattern))
         (uri-pattern (if question-mark-position
                          (subseq pattern 0 question-mark-position)
                        pattern))
         (variables-pattern (when question-mark-position
                              (subseq pattern (1+ question-mark-position)))))
    `(ensure-rest-resource-handler (make-instance 'rest-resource-handler
                                                  :name ',name
                                                  :pattern ',uri-pattern
                                                  :variables ',variables-pattern
                                                  :methods ',rest
                                                  :documentation ,docstring))))

(defun match-uri (uri pattern)
  (when (eql (length uri) (length pattern)) 
    (let (args)
      (loop :for pattern-component :in pattern
            :for uri-component :in uri
            :do (cond ((stringp pattern-component) 
                       (unless (string-equal pattern-component uri-component)
                         (return-from match-uri nil)))
                      ((eql (first pattern-component) :string)
                       (push uri-component args))
                      ((eql (first pattern-component) :integer)
                       (push (parse-integer uri-component :junk-allowed t) args))
                      ((eql (first pattern-component) :float)
                       (let ((float (read-from-string uri-component)))
                         (assert (numberp float))
                         float))
                      ((eql (first pattern-component) :boolean)
                       (push (string-equal uri-component "true") args))
                      ((eql (first pattern-component) :datetime)
                       (push (iso-8601-gmt->universal-time uri-component) args))
                      (t
                       (error "unknown pattern ~s" pattern-component))))
      (values t (nreverse args)))))

(defun find-matching-resource (uri)
  (loop :for resource :in *rest-resource-handlers*
        :do (multiple-value-bind (match-p args)
                (match-uri uri (get-pattern resource))
              (when match-p
                (return-from find-matching-resource (values resource args)))))
  nil)

(defun get-rest-resource-handler (name)
  (find name *rest-resource-handlers* :key #'get-name))

(defun describe-rest-resource (name)
  (let ((resource (get-rest-resource-handler name)))
    (when resource
      (format t "REST Resource~%NAME    ~s~%DOC     ~s~%PATTERN " (get-name resource) (get-documentation resource))
      (loop :for component :in (get-pattern resource)
            :do (let ((*print-case* :downcase)) 
                  (format t "/~a" component)))
      (when (get-variables resource)
        (let ((*print-case* :downcase)) 
          (format t "?~{~a~^&~}" (get-variables resource))))
      (terpri)
      (loop :for method :in (get-methods resource)
            :do (format t "~7,,,' a ~s~%" (first method) (second method)))
      (values))))

;; server REST interface 

(defgeneric get-rest-uri (object)
  (:documentation "Returns an URI string for object"))

;; args are the resolved argument value from the URI
;; in is an s-expr sent by the client

(defgeneric rest-get (resource context args)
  (:documentation "GET resource with args"))

(defgeneric rest-put (resource context args in)
  (:documentation "PUT resource with args and in"))

(defgeneric rest-post (resource context args in)
  (:documentation "POST resource with args and in"))

(defgeneric rest-delete (resource context args)
  (:documentation "DELETE resource with args"))

;; normally s-expr should be returned, the following escape is allowed:
;; (:custom <status-code> <reason> <mime-type> <content-string>)

(defclass rest-context ()
  ())

(defgeneric get-query (context)
  (:documentation "Returns a dotted alist of URI query key and value strings"))

(defgeneric get-headers (context)
  (:documentation "Returns a dotted alist of header key and value strings"))

;; Conditions

(define-condition rest-exception (error)
  ((code :reader get-code :initarg :code :initform nil)
   (message :reader get-message :initarg :message :initform nil)
   (arguments :reader get-arguments :initarg :arguments :initform nil))
  (:report (lambda (condition stream)
             (format stream "REST Exception:~@[ ~a~]~@[ ~a~] ~{~a~^, ~}" 
                     (get-message condition) (get-code condition) (get-arguments condition)))))

(define-condition rest-exceptions (error)
  ((rest-exceptions :reader get-rest-exceptions :initarg :rest-exceptions :initform nil))
  (:report (lambda (condition stream)
             (let ((rest-exceptions (get-rest-exceptions condition)))
               (case (length rest-exceptions)
                 (0 (format stream "REST Exception"))
                 (1 (format stream "~a" (first rest-exceptions)))
                 (t (format stream "REST Exceptions: ~{~a~^; ~}" rest-exceptions)))))))

(define-condition resource-not-found (error)
  ((uri :reader get-uri :initarg :uri :initform nil))
  (:report (lambda (condition stream)
             (format stream "Resource not found: ~a" (get-uri condition)))))

(defun resource-not-found (format-string &rest args)
  (make-condition 'resource-not-found :uri (apply #'format nil format-string args)))

(define-condition method-not-allowed (error)
  ((method :reader get-method :initarg :method :initform nil)
   (uri :reader get-uri :initarg :uri :initform nil))
  (:report (lambda (condition stream)
             (format stream "Resource not found: ~a" (get-uri condition)))))

(defun method-not-allowed (method format-string &rest args)
  (make-condition 'method-not-allowed :mehod method :uri (apply #'format nil format-string args)))

;; REST server implementation (based on S-HTTP-SERVER)

(defun render-s-expr (http-request stream s-expr status reason)
  (if (and (consp s-expr) (eql (car s-expr) :custom))
      (destructuring-bind (status reason mime-type content)
          (cdr s-expr)
        (s-http-server:write-http-response-status-line stream status reason)
        (s-http-server:write-http-response-headers 
         (s-http-server:standard-http-response-headers http-request
                                                       :content-type mime-type
                                                       :content-length (length content))
         stream)
        (s-http-server:write-http-response-line "" stream)
        (write-string content stream))
    (let ((content (unless (eql s-expr :void)
                     (with-output-to-string (out)
                       (print-s-expr s-expr out)
                       (terpri out)))))
      (s-http-server:write-http-response-status-line stream status reason)
      (s-http-server:write-http-response-headers 
       (s-http-server:standard-http-response-headers http-request
                                                     :content-type +s-expr-mime-type+
                                                     :content-length (if (eql s-expr :void) 0 (length content)))
       stream)
      (s-http-server:write-http-response-line "" stream)
      (unless (eql s-expr :void)
        (write-string content stream)))))

(defun parse-s-expr (http-request stream)
  (let* ((content-length-string (cdr (assoc "Content-Length" (s-http-server::get-headers http-request) 
                                            :test #'string-equal)))
         (content-length (s-utils:parse-integer-safely content-length-string))
         (buffer (when content-length (make-string content-length)))
         (content-type (cdr (assoc "Content-Type" (s-http-server::get-headers http-request) 
                                   :test #'string-equal))))
    ;; if no content-length is specified, we do not read anything!
    (when buffer
      (read-sequence buffer stream)
      (if (equal content-type +s-expr-mime-type+)
          (with-input-from-string (in buffer)
            (read-s-expr in))
        `(:custom ,content-type ,buffer)))))

(defclass s-http-rest-server-context (rest-context)
  ((http-request :accessor get-http-request :initarg :http-request)))

(defun parse-query (query)
  (mapcar #'(lambda (mapping)
              (let* ((tokens (s-utils:tokens mapping :separators (list #\=)))
                     (key (first tokens))
                     (value (second tokens)))
                (if value
                    (cons key (s-http-client:uri-decode-for-query (substitute #\Space #\+ value)))
                  (cons key nil))))
          (s-utils:tokens query :separators (list #\&))))

(defmethod get-query ((s-http-rest-server-context s-http-rest-server-context))
  (parse-query (puri:uri-query (s-http-server:get-uri (get-http-request s-http-rest-server-context)))))

(defmethod get-headers ((s-http-rest-server-context s-http-rest-server-context))
  (s-http-server:get-headers (get-http-request s-http-rest-server-context)))

(defun make-context (http-request)
  (make-instance 's-http-rest-server-context :http-request http-request))

(defun handle-rest-request-internal (http-request stream resource-name method args)
  (case method
    (:get (let ((s-expr (rest-get resource-name (make-context http-request) args)))
            (render-s-expr http-request stream s-expr 200 "OK")))
    (:post (let ((s-expr (rest-post resource-name (make-context http-request) args 
                                    (parse-s-expr http-request stream))))
             (if (stringp s-expr)
                 (progn
                   (s-http-server:write-http-response-status-line stream 202 "Created")
                   (s-http-server:write-http-response-headers 
                    (append
                     (s-http-server:standard-http-response-headers http-request
                                                                   :content-type +s-expr-mime-type+
                                                                   :content-length (+ 2 (length s-expr)))
                     `(("Location" . ,s-expr)))
                    stream)
                   (s-http-server:write-http-response-line "" stream)
                   (print-s-expr s-expr stream)
                   (terpri stream))
               (render-s-expr http-request stream (or s-expr :void) 202 "Created"))))
    (:put (let ((s-expr (rest-put resource-name (make-context http-request) args 
                                  (parse-s-expr http-request stream))))
            (render-s-expr http-request stream (or s-expr :void) 200 "OK")))
    (:delete (let ((s-expr (rest-delete resource-name (make-context http-request) args)))
               (render-s-expr http-request stream (or s-expr :void) 200 "OK")))
    (t (error "unknown method ~s" method))))

(defun compute-sub-path (uri-path context-prefix)
  "For an uri-path '/d1/d2/d3/d4/f' and a context '/d1/d2', return '('d3' 'd4' 'f')"
  (let* ((uri-path-components (s-utils:tokens uri-path :separators '(#\/)))
         (context-prefix-components (s-utils:tokens context-prefix :separators '(#\/)))
         (difference (mismatch context-prefix-components uri-path-components :test #'string=)))
    (when difference 
      (subseq uri-path-components difference))))

(defvar *server-debug-mode* nil
  "When t, exceptions are sent to the debugger on the server")

(defun s-http-server-rest-handler (s-http-server handler http-request stream)
  "Handler that implements the REST protocol"
  (declare (ignore s-http-server))
  (let* ((method (s-http-server:get-method http-request))
         (path (s-http-server:get-path http-request))
         (resource-uri (compute-sub-path path (first handler))))
    (multiple-value-bind (resource args)
        (find-matching-resource resource-uri)
      (if resource
          (progn
            (when *server-debug-stream*
              (format *server-debug-stream* ";; REST ~a ~a ~a~%;; REST ~a~@[ ~s~]~%" 
                      (s-utils:format-iso-gmt-time (get-universal-time)) method (s-http-server:get-uri http-request)
                      (get-name resource) args))
            (handler-bind ((resource-not-found 
                            #'(lambda (condition) 
                                (render-s-expr http-request stream 
                                               `("404" ,(princ-to-string condition)) 
                                               404 "Resource Not Found")))
                           (method-not-allowed
                            #'(lambda (condition) 
                                (render-s-expr http-request stream 
                                               `("405" ,(princ-to-string condition)) 
                                               404 "Method Not Allowed")))
                           (error 
                            #'(lambda (condition) 
                                (if *server-debug-mode*
                                    (invoke-debugger condition)
                                  (progn
                                    (when *server-debug-stream*
                                      (format *server-debug-stream* ";; REST Error: ~s~%" condition))
                                    (render-s-expr http-request stream 
                                                   `("500" ,(princ-to-string condition)) 
                                                   500 "Internal Server Error"))))))
              (if (member method (get-methods resource) :key #'first)
                  (handle-rest-request-internal http-request stream (get-name resource) method args)
                (method-not-allowed method resource-uri))))
        (s-http-server:standard-http-html-error-response http-request stream 
                                                         404 "Resource Not Found" 
                                                         (s-http-server:get-path http-request))))))

(defun make-s-http-rest-server (&key (port 1701))
  "Make an actual S-HTTP-SERVER with the REST handler installed"
  (let ((s-http-server (s-http-server:make-s-http-server :port port :name "s-http-rest-server")))
    (push `(s-http-server-rest-handler "/")
          (s-http-server:get-contexts s-http-server))
    s-http-server))

;; client REST call interface and implementation (based on S-HTTP-CLIENT)

(defun value->string (value)
  (if (stringp value) 
      value 
    (princ-to-string value)))

(defun build-rest-call-uri (pattern args &optional variables)
  (with-output-to-string (out)
    (let (consumed-args)
      (loop :for pattern-component :in pattern
            :do (cond ((stringp pattern-component) (format out "/~a" pattern-component))
                      (t (let* ((type (first pattern-component))
                                (name (second pattern-component))
                                (value (getf args name)))
                           (cond (value (format out "/~a" (case type
                                                            (:boolean (if value "true" "false"))
                                                            (:datetime (universal-time->iso-8601-gmt value))
                                                            (t (s-http-client:uri-encode-for-query 
                                                                (value->string value)))))
                                        (push name consumed-args))
                                 (t (error "uri pattern component variable ~s not bound" name)))))))
      (let ((*print-case* :downcase)
            variable-bindings)
        (loop :for variable :in variables :do
              (let* ((name (if (consp variable) (second variable) variable))
                     (type (if (consp variable) (first variable) :any))
                     (value (getf args name :not-found)))
                (unless (eql value :not-found)
                  (push (list name (case type
                                     (:boolean (if value "true" "false"))
                                     (:datetime (universal-time->iso-8601-gmt value))
                                     (t (s-http-client:uri-encode-for-query (value->string value)))))
                        variable-bindings)
                  (push name consumed-args))))
        (let ((free-args (set-difference (s-expr-get-keys args) consumed-args :test #'string-equal)))
          (loop :for free-arg :in free-args :do 
                (push (list free-arg (s-expr-getf args free-arg))
                      variable-bindings)))
        (when variable-bindings  
          (write-char #\? out)
          (loop :for (name value) :in (nreverse variable-bindings)
                :for n :upfrom 0
                :do 
                (unless (zerop n) (write-char #\& out))
                (format out "~a=~a" name value)))))))

(defun extract-rest-exceptions (content-string)
  (ignore-errors
    (let* ((s-expr (with-input-from-string (in content-string)
                     (read-s-expr in)))
           rest-exceptions)
      (loop :for sub-s-expr :in s-expr :do
            (let ((message (s-expr-getf sub-s-expr "message"))
                  (code (s-expr-getf sub-s-expr "code"))
                  (arguments (s-expr-getf sub-s-expr "arguments")))
              (when (or message code arguments)
                (push (make-condition 'rest-exception :message message :code code :arguments arguments)
                      rest-exceptions))))
      (when rest-exceptions
        (make-condition 'rest-exceptions :rest-exceptions rest-exceptions)))))

(defvar *connect-timeout* nil)
(defvar *read-timeout* nil)
(defvar *write-timeout* nil)

(defun do-rest-call-internal (url method &optional request-content mime-type)
  (multiple-value-bind (response-content response-code headers uri)
      (s-http-client:do-http-request url 
                                     :method method
                                     :content-type (when request-content (or mime-type +s-expr-mime-type+))
                                     :content request-content
                                     :connect-timeout *connect-timeout*
                                     :read-timeout *read-timeout*
                                     :write-timeout *write-timeout*)
    (let* ((content-length-string (cdr (assoc :content-length headers)))
           (content-length (when content-length-string
                             (parse-integer content-length-string :junk-allowed t)))
           (content-type (cdr (assoc :content-type headers))))
      (case response-code 
        ((200 201 202) (unless (or (equal response-content "") 
                                   (and content-length (zerop content-length)))
                     (if (equal (search "text/s-exp" content-type) 0)
                         (with-input-from-string (in response-content)
                           (read-s-expr in))
                       (values response-content (cdr (assoc :content-type headers))))))
        (t (let ((rest-exceptions (extract-rest-exceptions response-content)))
             (error (or rest-exceptions
                        (if (eql response-code 404)
                            (make-condition 'resource-not-found :uri uri)
                          (make-condition 'simple-error 
                                          :format-control "http ~s request ~a failed with code ~d" 
                                          :format-arguments (list method uri response-code)))))))))))

(defun do-rest-call (resource-spec host method args &optional in)
  (let* ((start (get-internal-real-time))
         (uri (if (stringp resource-spec)
                  resource-spec
                (let ((resource (get-rest-resource-handler resource-spec)))
                  (if resource
                      (if (member method (get-methods resource) :key #'first)
                          (build-rest-call-uri (get-pattern resource) args (get-variables resource))
                        (error "method ~s not allowed on resource ~s" method (get-name resource)))
                    (error "cannot find rest resource named ~s" resource-spec)))))
         (url (concatenate 'string host uri))
         content
         mime-type)
    (when (member method '(:put :post))
      (cond ((and (consp in) (eql (car in) :custom))
             (setf content (third in)
                   mime-type (second in)))
            (t
             (setf content (with-output-to-string (out)
                             (print-s-expr in out))
                   mime-type +s-expr-mime-type+))))
    (when *debug-stream* 
      (format *debug-stream* ";; REST ~a ~a ~a" (s-utils:format-iso-gmt-time (get-universal-time)) method url))
    (unwind-protect
        (do-rest-call-internal url method content mime-type)
      (when *debug-stream* 
        (let ((execution-time (- (get-internal-real-time) start)))
          (if (< execution-time 1000)
              (format *debug-stream* " ~dms~%" execution-time)
            (format *debug-stream* " ~fs~%" (/ execution-time 1000.0))))))))
     
(defvar *default-host* nil)

(defmacro with-host ((host) &body body)
  `(let ((*default-host* ,host))
     ,@body))

(defun do-get (resource-spec &optional args &key (host *default-host*))
  (do-rest-call resource-spec host :get args))

(defun do-post (resource-spec in &optional args &key (host *default-host*))
  (do-rest-call resource-spec host :post args in))

(defun do-put (resource-spec in &optional args &key (host *default-host*))
  (do-rest-call resource-spec host :put args in))

(defun do-delete (resource-spec &optional args &key (host *default-host*))
  (do-rest-call resource-spec host :delete args))

;;;; eof
