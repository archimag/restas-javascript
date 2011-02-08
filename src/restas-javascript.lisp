;;;; restas-javascript.cl
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun js-prop-or (obj prop &optional default)
  (if (eql obj :undefined)
      default
      (let ((value (js-prop obj prop)))
        (if (eq value :undefined)
            default
            value))))

(defun lisp-to-camel-case (obj)
  "Take a string with Lisp-style hyphentation and convert it to camel
case.  This is an inverse of CAMEL-CASE-TO-LISP."
  (if (stringp obj)
      obj
      (let ((string (string-downcase obj)))
        (loop with i = 0 and l = (length string)
           with cc-string = (make-string l) and cc-i = 0
           with init = t and cap = nil and all-caps = nil
           while (< i l)
           do (let ((c (aref string i)))
                (unless (case c
                          (#\* (if init (setq cap t)))
                          (#\+ (cond
                                 (all-caps (setq all-caps nil init t))
                                 (init (setq all-caps t))))
                          (#\- (progn
                                 (setq init t)
                                 (cond
                                   ((or all-caps
                                        (and (< (1+ i) l)
                                             (char= (aref string (1+ i)) #\-)
                                             (incf i)))
                                    (setf (aref cc-string cc-i) #\_)
                                    (incf cc-i))
                                   (t (setq cap t))))))
                  (setf (aref cc-string cc-i)
                        (if (and (or cap all-caps) (alpha-char-p c))
                            (char-upcase c)
                            (char-downcase c)))
                  (incf cc-i)
                  (setq cap nil init nil))
                (incf i))
           finally (return (subseq cc-string 0 cc-i))))))


(defun alist-js-obj (alist)
  (let ((obj (js-obj)))
    (iter (for (key . value) in alist)
          (setf (js-prop obj (lisp-to-camel-case key))
                value))
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-js-obj (module (:constructor make-module (cls)))
  routes submodules)

(defclass submodule ()
  ((module :initarg :module)
   (baseurl :initarg :baseurl)
   (environmet :initarg :environment)))

(define-js-obj (route (:constructor make-route (cls)))
  url method handler)

(defun init-route (route props)
  (setf (slot-value route 'url) (js-prop-or props "url" "")
        (slot-value route 'method) (string-upcase (js-prop-or props "method" "GET"))
        (slot-value route 'handler) (js-prop route "handler"))
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

(defmethod restas:process-route ((route route-wrap) bindings)
  (let* ((sub (slot-value route 'submodule))
         (*env* (slot-value sub 'environmet))
         (context (js-obj)))
    (setf (js-prop context "request")
          (new-request))
    (setf (js-prop context "reply")
          (new-reply))
    (js-call (js-prop (slot-value route 'jsroute) 
                      "handler")
             context
             (alist-js-obj bindings))))


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

      (.func "start" (args)
        (when (module-p this)
          (restas:start this
                        :port (js-prop-or args "port" 8080)))))

    (.prototype :request
      (.func "realRemoteAddr" ()
        (hunchentoot:real-remote-addr (request-%request this)))
      (.func "cookie" (name)
        (hunchentoot:cookie-in name (request-%request this)))
      (.func "host" ()
        (hunchentoot:host (request-%request this)))
      (.func "queryString" ()
        (hunchentoot:query-string (request-%request this)))
      (.func "referer" ()
        (hunchentoot:referer (request-%request this)))
      (.func "method" ()
        (hunchentoot:request-method (request-%request this)))
      (.func "uri" ()
        (hunchentoot:request-uri (request-%request this)))
      (.func "serverProtocol" ()
        (hunchentoot:server-protocol (request-%request this)))
      (.func "userAgent" ()
        (hunchentoot:user-agent (request-%request this)))
      (.func "header" (name)
        (hunchentoot:header-in name (request-%request this)))
      (.func "remoteAddr" ()
        (hunchentoot:remote-addr (request-%request this)))
      (.func "remotePort" ()
        (hunchentoot:remote-port (request-%request this)))
      (.func "scriptName" ()
        (hunchentoot:script-name (request-%request this))))

    (.prototype :reply
      (.func "header" (name value)
        (cond
          ((eql value :undefined)
           (hunchentoot:header-out name (reply-%reply this)))
          (t (setf (hunchentoot:header-out name (reply-%reply this))
                   value))))
      (.active "contentLength"
        (:read () (hunchentoot:content-length (reply-%reply this)))
        (:write (value)
          (setf (hunchentoot:content-length* (reply-%reply this))
                value)))
      (.active "contentType"
        (:read () (hunchentoot:content-type (reply-%reply this)))
        (:write (value)
           (setf (hunchentoot:content-type* (reply-%reply this))
                 value)))
      (.func "cookie" (name value props)
        (cond
          ((eql value :undefined)
           (hunchentoot:cookie-out name (reply-%reply this)))
          (t (hunchentoot:set-cookie name
                                     :value value
                                     :expires (js-prop-or props "expires")
                                     :path (js-prop-or props "path")
                                     :domain (js-prop-or props "domain")
                                     :secure (js-prop-or props "secure")
                                     :http-only (js-prop-or props "httpOnly")
                                     :reply (reply-%reply this)))))
      (.active "returnCode"
        (:read () (hunchentoot:return-code (reply-%reply this)))
        (:write (value)
          (setf (hunchentoot:return-code (reply-%reply this))
                value))))

    (.func "reconnectAllRoutes" ()
      (restas:reconnect-all-routes))))

(add-to-lib *restas-js-lib*
  (.object "console"
    (.func "log" (obj)
      (format t "~A~%" (to-string obj))
      (values))))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; environmetn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *restas-js-env* (create-env *restas-js-lib*))

(defgeneric execute (obj &key &allow-other-keys)
  (:documentation "Execute JavaScript"))

(defmethod execute :around (obj &key (compile t) wrap-parse-errors optimize wrap-as-module)
  (let ((*env* *restas-js-env*))
    (call-next-method obj
                      :compile compile
                      :wrap-parse-errors wrap-parse-errors
                      :optimize optimize
                      :wrap-as-module wrap-as-module)))

(defmethod execute ((str string) &key (compile t) wrap-parse-errors optimize wrap-as-module)
  (run-js str
          :compile compile
          :wrap-parse-errors wrap-parse-errors
          :optimize optimize
          :wrap-as-module wrap-as-module))
          
(defmethod execute ((file pathname) &key (compile t) wrap-parse-errors optimize wrap-as-module)
  (run-js-file file 
               :compile compile
               :wrap-parse-errors wrap-parse-errors
               :optimize optimize
               :wrap-as-module wrap-as-module))

(defun repl ()
  (let ((*env* *restas-js-env*))
    (js-repl)))