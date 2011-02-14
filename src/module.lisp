;;;; module.cl
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

(define-js-obj (%module (:constructor make-%module (cls)))
  routes submodules context)

(defun init-%module (module)
  (setf (%module-routes module) (js-obj)
        (%module-context module) (js-obj)
        (%module-submodules module) (js-obj))
  module)

(defun copy-js-obj (source dist)
  (js-for-in dist
             (lambda (name)
               (delete-prop dist name)))
  (js-for-in source
             (lambda (name)
               (setf (js-prop dist name)
                     (js-prop source name))))
  dist)
  
(defclass submodule (restas:submodule)
  ((baseurl :initform "" :initarg :baseurl :reader restas:submodule-baseurl)
   (environmet :initarg :environment)))

(defmethod restas:make-submodule ((module %module) &key &allow-other-keys)
  (make-instance 'submodule
                 :module module
                 :environment *env*))

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
            (%module-context (%route-module route))))))

(defmethod restas:module-routes ((module %module) (submodule submodule))
  (let ((routes (slot-value module 'routes))
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


                 
  