;;;; routes.lisp
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

(define-js-obj (%route (:constructor make-%route (cls)))
  url method handler module)

(defclass route (routes:route)
  ((%route :initarg :%route)
   (submodule :initarg :submodule)))

;;;; Check conditions

(defmethod routes:route-check-conditions ((route route) bindings)
  (string= (hunchentoot:request-method*)
           (%route-method (slot-value route '%route))))

;;;; Process route

(define-js-obj (request (:constructor make-request (cls)))
  %request)

(defun new-request ()
  (let ((request (js-obj :request 'request)))
    (setf (request-%request request)
          hunchentoot:*request*)
    (setf (js-prop request "post")
          (alist-js-obj (hunchentoot:post-parameters*)))
    (setf (js-prop request "get")
          (alist-js-obj (hunchentoot:get-parameters*)))
    request))

(define-js-obj (reply (:constructor make-reply (cls)))
  %reply)

(defun new-reply ()
  (let ((reply (js-obj :reply 'reply)))
    (setf (reply-%reply reply)
          hunchentoot:*reply*)
    reply))

(defmethod restas:process-route ((route route) bindings)
  (let* ((sub (slot-value route 'submodule))
         (*env* (slot-value sub 'environmet))
         (context (js-obj)))
    (setf (js-prop context "request")
          (new-request))
    (setf (js-prop context "reply")
          (new-reply))
    (setf (js-prop context "context")
          (js-obj (module-context (slot-value sub 'module))))
    (js-call (js-prop (slot-value route '%route) 
                      "handler")
             context
             (alist-js-obj bindings))))
