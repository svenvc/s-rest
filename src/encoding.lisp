;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; Basic encoding/decoding routines and s-expr related stuff for the REST framework
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-rest)

(defconstant +s-expr-mime-type+ "text/s-expr")

;; string encoding and decoding

(defun encode-string (string &optional stream)
  "Encode a string to a stream (or nil for string output) using basic esapce conventions"
  (let ((out (or stream (make-string-output-stream))))
    (loop :for char :across string 
          :do (let ((code (char-code char)))
                (cond ((char= char #\") (write-string "\\\"" out))
                      ((char= char #\\) (write-string "\\\\" out))
                      ((char= char #\newline) (write-string "\\n" out))
                      ((char= char #\return) (write-string "\\r" out))
                      ((char= char #\formfeed) (write-string "\\f" out))
                      ((char= char #\tab) (write-string "\\t" out))
                      ((char= char #\backspace) (write-string "\\b" out))
                      ((char= char #\null) (write-string "\\0" out))
                      ((<= 32 code 126) (write-char char out))
                      ((< code 256) (format out "\\x~2,'0X" code))
                      (t (format out "\\u~4,'0X" code)))))
    (unless stream
      (get-output-stream-string out))))

(defun decode-string (stream)
  "Decode a string from stream using basic escape conventions (and some optional ones)"
  (let ((buffer (make-string-output-stream)))
    (loop :for char = (read-char stream nil)
          :until (or (null char) (char= char #\"))
          :do (if (char= char #\\)
                  (progn
                    (setf char (read-char stream nil))
                    (when (null char) (error "missing escape character"))
                    (case char
                      ((#\" #\\) (write-char char buffer))
                      (#\n (write-char #\newline buffer))
                      (#\r (write-char #\return buffer))
                      (#\f (write-char #\formfeed buffer))
                      (#\t (write-char #\tab buffer))
                      (#\b (write-char #\backspace buffer))
                      (#\0 (write-char #\null buffer))
                      (#\u (let ((digit-buffer (make-string 4)))
                             (unless (= (read-sequence digit-buffer stream) 4)
                               (error "not enough hex digits for unicode escape sequence"))
                             ;; switch to from base-char if necessary
                             (when (eql (stream-element-type stream) 'base-char)
                               (let ((unicode-buffer (make-string-output-stream :element-type 'lw:simple-char)))
                                 (write-sequence (get-output-stream-string buffer) unicode-buffer)
                                 (setf buffer unicode-buffer)))
                             (write-char (code-char (parse-integer digit-buffer :radix 16)) buffer)))
                      (#\x (let ((digit-buffer (make-string 2)))
                             (unless (= (read-sequence digit-buffer stream) 2)
                               (error "not enough hex digits for latin-1 escape sequence"))
                             (write-char (code-char (parse-integer digit-buffer :radix 16)) buffer)))
                      (t (error "unsupported escape character ~c" char))))
                (write-char char buffer)))
    (get-output-stream-string buffer)))

;; datetime encoding and decoding

(defun universal-time->iso-8601-gmt (universal-time)
  "Convert a Common Lisp universal time to a full 15 char ISO 8601 GMT string 'YYYYMMDDTHHMMSS'"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" 
            year month date hour minute second)))

(defun iso-8601-gmt->universal-time (string)
  "Convert a full 15 char ISO 8601 GMT string 'YYYYMMDDTHHMMSS' to a Common Lisp universal time"
  (if (= 15 (length string))
      (let ((year (parse-integer string :start 0 :end 4))
            (month (parse-integer string :start 4 :end 6))
            (date (parse-integer string :start 6 :end 8))
            (hour (parse-integer string :start 9 :end 11))
            (minute (parse-integer string :start 11 :end 13))
            (second (parse-integer string :start 13 :end 15)))
        (encode-universal-time second minute hour date month year 0))
    (error "String ~s too short to represent a full 15 char ISO 8601 GMT time" string)))

;; s-expr input and output

(defun print-s-expr (s-expr &optional (out *standard-output*))
  "Write s-expr to out using our conventions"
  (cond ((null s-expr) (write-string "()" out))
        ((integerp s-expr) (format out "\"~a\"" s-expr)) 
        ((realp s-expr) (format out "\"~f\"" s-expr))
        ((consp s-expr) 
         (write-char #\( out)
         (loop :for x :in s-expr
               :for i :upfrom 0
               :do 
               (unless (zerop i) (write-char #\space out))
               (print-s-expr x out))
         (write-char #\) out))
        (t (write-char #\" out)
           (encode-string (princ-to-string s-expr) out)
           (write-char #\" out))))

(defun is-whitespace-char (char)
  (or (char= char #\space) (char= char #\newline) (char= char #\return) (char= char #\tab)))

(defun read-s-expr (in &optional (eof-error-p t) eof-value)
  "Read an s-expr from in using our conventions"
  (let ((char (read-char in nil)))
    (loop :while (and char (is-whitespace-char char)) :do (setf char (read-char in nil)))
    (cond ((null char) (if eof-error-p 
                           (error "read eof instead of a valid s-expr")
                         eof-value))
          ((char= char #\") (decode-string in))
          ((char= char #\() (let (elements)
                              (loop :for char = (peek-char nil in nil) :do
                                    (cond ((null char) (if eof-error-p
                                                           (error "read eof; expected list element / closing parenthesis")
                                                         (return eof-value)))
                                          ((is-whitespace-char char) (read-char in))
                                          ((char= char #\)) (read-char in) (return))
                                          (t (push (read-s-expr in) elements))))
                              (nreverse elements)))
          (t (error "unexpected s-expr character ~s in input" char)))))

;; working with the test file:

#+nil
(with-open-file (in "~/darcs/s-rest/test/test.s-expr")
  (loop :for x = (read-s-expr in nil :eof) 
        :until (eql x :eof) 
        :collect x))

#+nil
(dolist (x *) (print-s-expr x *standard-output*) (terpri) (terpri))

;; s-expr representation 

(defclass s-expr-representation ()
  ((name :accessor get-name :initarg :name)
   (parent :accessor get-parent :initarg :parent)
   (slots :accessor get-slots :initarg :slots)
   (documentation :accessor get-documentation :initarg :documentation :initform nil)))

(defvar *s-expr-representations* '())

(defun ensure-s-expr-representation (representation)
  (if (member (get-name representation) *s-expr-representations* :key #'get-name)
      (setf *s-expr-representations* (delete (get-name representation) *s-expr-representations* :key #'get-name)))
  (push representation *s-expr-representations*))

(defun resolve-type-references (string &optional (resolver #'identity))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop :for char = (read-char in nil)
            :while char
            :do (if (char= char #\<)
                    (let ((type-reference 
                           (with-output-to-string (out)
                             (loop 
                              (setf char (read-char in))
                              (cond ((char= char #\>) (return))
                                    (t (write-char char out)))))))
                      (write-string (funcall resolver type-reference) out))
                  (write-char char out))))))

(defmacro define-s-expr-representation (name parent docstring slots)
  `(ensure-s-expr-representation (make-instance 's-expr-representation
                                                :name ',name
                                                :parent ',(car parent)
                                                :documentation ,docstring
                                                :slots ',slots)))

(defun get-s-expr-representation (name)
  (find name *s-expr-representations* :key #'get-name))

(defun describe-s-expr-representation (name)
  (let ((s-expr-representation (get-s-expr-representation name)))
    (when s-expr-representation
      (format t "REST Representation~%NAME    ~s~%DOC     ~s~%~@[PARENT  ~s~%~]" 
              (get-name s-expr-representation) (get-documentation s-expr-representation) 
              (get-parent s-expr-representation))
      (loop :for (slot-name type documentation) :in (get-slots s-expr-representation)
            :do (let ((*print-case* :downcase)) 
                  (format t "~16,,,' a ~16,,,' s ~s~%" slot-name type documentation)))
      (values))))

;; s-expr support routines

(defun hash-table->s-expr (hash-table &key (downcase-keys t))
  "Convert a hash-table to an s-expression"
  (loop :for key :being :the :hash-keys :in hash-table
        :for value :being :the :hash-values :in hash-table
        :append (list (if downcase-keys 
                          (string-downcase (princ-to-string key))
                        (princ-to-string key))
                      (princ-to-string value))))

(defun object->s-expr (object slots &key (downcase-keys t))
  "Convert an object's slots to an s-expression"
  (loop :for slot-spec :in slots
        :append (let* ((slot (if (consp slot-spec) (first slot-spec) slot-spec))
                       (type (when (consp slot-spec) (second slot-spec))))
                  (when (and (slot-exists-p object slot) (slot-boundp object slot))
                    (list (if downcase-keys
                              (string-downcase (symbol-name slot))
                            (symbol-name slot))
                          (case type
                            (:datetime (universal-time->iso-8601-gmt (slot-value object slot)))
                            (:boolean (if (slot-value object slot) "true" "false"))
                            (:string (slot-value object slot))
                            (t (princ-to-string (slot-value object slot)))))))))

(defun s-expr->hash-table (s-expr &optional (hash-table (make-hash-table)))
  "Convert an s-expression to a hash-table"
  (loop :for head :on s-expr :by #'cddr
        :do (setf (gethash (car head) hash-table) (cadr head)))
  hash-table)

(defun map-s-expr (function s-expr)
  "Map a two-argument function to all key/value pairs in s-expr"
  (loop :for head :on s-expr :by #'cddr
        :do (funcall function (car head) (cadr head))))

(defun s-expr->object (s-expr class slots)
  "Convert an s-expression to an object of class using slots"
  (let ((object (make-instance class)))
    (loop :for slot-spec :in slots
          :do (let* ((slot (if (consp slot-spec) (first slot-spec) slot-spec))
                     (type (when (consp slot-spec) (second slot-spec)))
                     (head (member slot s-expr :test #'string-equal)))
                (when (and head (slot-exists-p object slot) (slot-boundp object slot))
                  (setf (slot-value object slot) 
                        (case type
                          (:integer (parse-integer (second head)))
                          (:float (let ((float (read-from-string (second head))))
                                    (assert (numberp float))
                                    float))
                          (:datetime (iso-8601-gmt->universal-time (second head)))
                          (:boolean (string-equal (second head) "true"))
                          (t (second head)))))))
    object))

(defgeneric to-s-expr (object))

(defgeneric from-s-expr (object s-expr))

(defun safe-string-equal (x y) 
  (and (stringp x) 
       (stringp y) 
       (string-equal x y)))

(defun s-expr-getf (s-expr key &optional default)
  "Extract the value stored with key in s-expr, return default if not found"
  (or (second (member key s-expr :test #'safe-string-equal))
      default))

(defun s-expr-setf (s-expr key value)
  "Modify or add the binding key-value in s-expr"
  (cond ((null s-expr) (list key value))
        ((safe-string-equal key (first s-expr)) (cons key 
                                                     (cons value 
                                                           (rest (rest s-expr))))) 
        (t (cons (first s-expr) 
                 (cons (second s-expr) 
                       (s-expr-setf (rest (rest s-expr)) 
                                    key 
                                    value))))))

(defun s-expr-replace (s-expr key value)
  "Destructively modify one binding key-value in s-expr, if present"
  (cond ((null s-expr))
        ((safe-string-equal key (first s-expr)) (setf (second s-expr) value))
        (t (s-expr-replace (rest (rest s-expr)) 
                           key 
                           value))))

(defun (setf s-expr-getf) (value s-expr key)
  "Modify or add the binding key-value in s-expr"
  (s-expr-setf s-expr key value))

(defun s-expr-remf (s-expr key)
  "Remove any binding under key in s-expr"
  (cond ((null s-expr) nil)
        ((safe-string-equal key (first s-expr)) (rest (rest s-expr)))
        (t (cons (first s-expr)
                 (cons (second s-expr)
                       (s-expr-remf (rest (rest s-expr))
                                    key))))))

(defun s-expr-select (s-expr keys)
  "Select a sub-s-expr for keys from s-expr"
  (if (null s-expr) 
      nil
    (if (member (first s-expr) keys :test #'safe-string-equal)
        (cons (first s-expr)
              (cons (second s-expr)
                    (s-expr-select (rest (rest s-expr))
                                   keys)))
      (s-expr-select (rest (rest s-expr))
                     keys))))

(defun s-expr-get-keys (s-expr)
  "Return all keys from s-expr"
  (when s-expr
    (cons (first s-expr) (s-expr-get-keys (rest (rest s-expr))))))

(defun s-expr-get-values (s-expr)
  "Return all values from s-expr"
  (when s-expr
    (cons (second s-expr) (s-expr-get-values (rest (rest s-expr))))))

(defun make-s-expr-sorter (key predicate &optional default)
  #'(lambda (x y)
      (funcall predicate (s-expr-getf x key default) (s-expr-getf y key default))))

(defun make-s-expr-key-equals-value (key value)
  #'(lambda (s-expr)
      (string-equal (s-expr-getf s-expr key) value)))

;;;; eof
