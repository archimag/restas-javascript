;;;; restas-javascript.lisp
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

(defparameter *restas-js-lib* (empty-lib "RESTAS"))

(add-to-lib *restas-js-lib*
  (.object "Restas"
    (.prototype :route
      (.active "url"
        (:read () (%route-url this))
        (:write (value) (prog1 (setf (%route-url this)
                                     value)
                          (restas:reconnect-all-routes))))
      (.active "method"
        (:read () (%route-method this))
        (:write (value) (prog1 (setf (%route-method this)
                                     value)
                          (restas:reconnect-all-routes))))
      (.active "handler"
        (:read () (%route-handler this))
        (:write (value) (set-%route-handler this value))))
    
    (.constructor "Module"  ()
      (init-module this)
      (:slot-default :noenum)
      (:make-new 'make-module)
      (:prototype :module))

    (.prototype :module
      (.active-r "routes"
        (if (module-p this)
            (module-routes this)
            :undefined))

      (.active "context"
        (:read () (module-context this))
        (:write (value) (set-module-context this value)))
      
      (.func "defineRoute" (args)
        (create-%route this args))

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