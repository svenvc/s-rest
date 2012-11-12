;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; An HTML documentation generation facility for REST resources and s-expr representations
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-rest)

(defun type-to-link (type)
  (with-html-output-to-string (out)
    (:a :href (format nil "#~a" type) (str (subseq type 1)))))

(defun generate-html-for-rest-resource (resource out)
  (with-html-output (out)
    (:div :style "margin:10px"
     (:a :name (format nil "-~a" (get-name resource))
      (:table :border 0 :width "100%" :cellpadding 2 :cellspacing 2
       (:tr (:td :colspan 2 :bgcolor "#CCCCCC" (:strong (fmt "~s" (get-name resource)))))
       (:tr (:td :colspan 2 (:tt 
                             (loop :for component :in (get-pattern resource)
                                   :do (let ((*print-case* :downcase)) 
                                         (htm (fmt "/~a" component))))
                             (when (get-variables resource)
                               (let ((*print-case* :downcase)) 
                                 (htm (fmt "?~{~a~^&~}" (get-variables resource))))))))
       (:tr (:td :colspan 2 (:em (str (resolve-type-references (get-documentation resource)
                                                               #'type-to-link)))))
       (loop :for (method-name method-documentation) :in (get-methods resource)
             :do (htm (:tr 
                       (:td :width "10%" (str method-name))
                       (:td (:em (str (resolve-type-references method-documentation
                                                               #'type-to-link))))))))))))

(defun generate-html-for-s-expr-representation (representation out)
  (with-html-output (out)
    (:div :style "margin:10px"
     (:a :name (format nil "+~a" (get-name representation))
      (:table :border 0 :width "100%" :cellpadding 2 :cellspacing 2
       (:tr (:td :colspan 3 :bgcolor "#CCCCCC" 
             (:strong (fmt "~s" (get-name representation)))
             (when (get-parent representation)
               (htm 
                " ( "
                (:a :href (format nil "#+~a" (get-parent representation)) 
                 (str (get-parent representation)))
                " ) "))))
       (:tr (:td :colspan 3 (:em (str (resolve-type-references (get-documentation representation)
                                                               #'type-to-link)))))
       (loop :for (slot-name slot-type slot-documentation) :in (get-slots representation)
             :do (htm (:tr 
                       (:td :width "20%" (str slot-name))
                       (:td :width "20%" (str slot-type))
                       (:td (:em (str (resolve-type-references slot-documentation
                                                               #'type-to-link))))))))))))

(defun resolve-tree (tree resolver)
  (when tree
    (cond ((symbolp tree) (funcall resolver tree))
          ((stringp (first tree)) (cons (first tree) 
                                        (remove-if #'null
                                                   (mapcar #'(lambda (tree) (resolve-tree tree resolver))
                                                           (rest tree)))))
          (t (remove-if #'null
                        (mapcar #'(lambda (tree) (resolve-tree tree resolver))
                                tree))))))

(defun generate-doc (tree out renderer)
  (with-html-output (out)
    (cond ((typep tree 'standard-object) (funcall renderer tree out))
          ((stringp (first tree))
           (htm
            :hr
            (:h3 (str (first tree)))
            (generate-doc (rest tree) out renderer)))
          (t (loop :for node :in tree
                   :do (generate-doc node out renderer))))))

(defun generate-toc (tree out prefix)
  (with-html-output (out)
    (cond ((typep tree 'standard-object) (let ((name (get-name tree)))
                                           (htm
                                            (:li
                                             (:a :href (format nil "#~a~a" prefix name) (str name))))))
          ((stringp (first tree)) (htm 
                                   (:li
                                    (str (first tree))
                                    (generate-toc (rest tree) out prefix))))
          (t (htm
              (:ul
               (loop :for node :in tree
                     :do (generate-toc node out prefix))))))))
    
(defun generate-html-for-rest-resources (directory resource-names representation-names)
  (let ((defaults (pathname directory))
        (resources (resolve-tree resource-names #'get-rest-resource-handler))
        (representations (resolve-tree representation-names #'get-s-expr-representation)))
    (with-open-file (out (merge-pathnames "rest-resources.html" defaults)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (with-html-output (out)
        (:html
         (:head (:title "REST API"))
         (:body 
          (:h1 "REST Resources")
          (generate-toc resources out "-")
          (generate-doc resources out #'generate-html-for-rest-resource)
          (:h1 "REST Representations")
          (generate-toc representations out "+")
          (generate-doc representations out #'generate-html-for-s-expr-representation)))))
    t))

;;;; eof
