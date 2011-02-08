;;;; module.cl
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

(define-js-obj (module (:constructor make-module (cls)))
  routes submodules context)

(defun init-module (module)
  (setf (module-routes module) (js-obj)
        (module-context module) (js-obj))
  module)

(defun set-module-context (module value)
  (let ((context (module-context module)))
    (js-for-in module
               (lambda (name)
                 (cl-js::delete-prop context name)))
    (js-for-in value
               (lambda (name)
                 (setf (js-prop context name)
                       (js-prop value name))))
    context))


(defclass submodule ()
  ((module :initarg :module)
   (baseurl :initarg :baseurl)
   (environmet :initarg :environment)))

(defmethod restas:make-submodule ((module module) &key &allow-other-keys)
  (make-instance 'submodule
                 :module module
                 :environment *env*
                 :baseurl ""))

(defun create-%route (module args)
  (let ((route (js-obj :route '%route)))
    (setf (%route-url route) (js-prop-or args "url" "")
          (%route-method route) (string-upcase (js-prop-or args "method" "GET"))
          (%route-handler route) (js-prop-or args "handler")
          (%route-module route) module)
    (let ((handler (js-prop-or args "handler")))
      (when handler
        (setf (%route-handler route)
              handler)
        (setf (js-prop handler "context")
              (js-prop module "context"))))
    route))

(defun set-%route-handler (route value)
  (prog1 (setf (%route-handler route)
               value)
    (unless (eql value :undefined)
      (setf (js-prop value "context")
            (module-context (%route-module route))))))


(defmethod restas:submodule-routes ((submodule submodule))
  (let* ((module (slot-value submodule 'module))
         (routes (slot-value module 'routes))
         routes-list)
    (flet ((handle-route (name)
             (let ((route (js-prop routes name)))
               (push (make-instance 'route
                                    :template (routes:parse-template (%route-url route))
                                    :%route route
                                    :submodule submodule)
                     routes-list))))
      (js-for-in routes #'handle-route)
      routes-list)))
