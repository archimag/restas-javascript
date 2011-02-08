;;;; module.cl
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

(define-js-obj (module (:constructor make-module (cls)))
  routes submodules)

(defclass submodule ()
  ((module :initarg :module)
   (baseurl :initarg :baseurl)
   (environmet :initarg :environment)))


(defmethod restas:make-submodule ((module module) &key &allow-other-keys)
  (make-instance 'submodule
                 :module module
                 :environment *env*
                 :baseurl ""))

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
