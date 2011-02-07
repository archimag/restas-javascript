;;;; restas-js.cl
;;;;
;;;; This file is part of the RESTAS-JS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas-js)

(defun js-prop-or (obj prop &optional default)
  (let ((value (js-prop obj prop)))
    (if (eq value :undefined)
        default
        value)))

(define-js-obj (module (:constructor make-module (cls)))
  routes submodules)

(defclass submodule ()
  ((module :initarg :module)
   (baseurl :initarg :baseurl)
   (environmet :initarg :environment)))

(define-js-obj (route (:constructor make-route (cls)))
  url method handler)

(defun init-route (route props)
  (flet ((js-prop-or (obj prop &optional default)
           (if (eql obj :undefined)
               default
               (let ((value (js-prop obj prop)))
                 (if (eq value :undefined)
                     default
                     value)))))
    (setf (slot-value route 'url) (js-prop-or props "url" "")
          (slot-value route 'method) (string-upcase (js-prop-or props "method" "GET"))
          (slot-value route 'handler) (js-prop route "handler")))
  route)


(defclass route-wrap (routes:route)
  ((jsroute :initarg :js-route)
   (submodule :initarg :submodule)))


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
               (push (make-instance 'route-wrap
                                    :template (routes:parse-template (route-url route))
                                    :js-route route
                                    :submodule submodule)
                     routes-list))))
      (js-for-in routes #'handle-route)
      routes-list)))

(defmethod routes:route-check-conditions ((route route-wrap) bindings)
  (string= (hunchentoot:request-method*)
           (route-method (slot-value route 'jsroute))))

(defmethod restas:process-route ((route route-wrap) bindings)
  (let* ((sub (slot-value route 'submodule))
         (*env* (slot-value sub 'environmet)))
    (js-call (js-prop (slot-value route 'jsroute) "handler") nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; restas-js-lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *restas-js-lib* (empty-lib "RESTAS"))

(add-to-lib *restas-js-lib*
  (.object "Restas"
    (.constructor "Route" (props)
      (init-route this props)
      (:make-new 'make-route)
      (:prototype :route))
    
    (.prototype :route
      (.active "url"
        (:read () (route-url this))
        (:write (value) (prog1 (setf (route-url this)
                                     value)
                          (restas:reconnect-all-routes))))
      (.active "method"
        (:read () (route-method this))
        (:write (value) (prog1 (setf (route-method this)
                                     value)
                          (restas:reconnect-all-routes))))
      (.active "handler"
        (:read () (route-handler this))
        (:write (value) (setf (route-handler this)
                                     value)
                )))
    
    (.constructor "Module"  ()
      (if (eq this *env*)
          this
          (progn
            (setf (module-routes this)
                  (js-obj))
            this))
      (:slot-default :noenum)
      (:make-new 'make-module)
      (:prototype :module))

    (.prototype :module
      (.active-r "routes"
        (if (module-p this)
            (module-routes this)
            :undefined))

      (.func "start" ()
        (when (module-p this)
          (restas:start this :port 8080))
        ))

    (.func "reconnectAllRoutes" ()
      (restas:reconnect-all-routes))

    ))



