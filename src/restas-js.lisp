;;;; restas-js.cl
;;;;
;;;; This file is part of the RESTAS-JS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas-js)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-js-obj (module (:constructor make-module (cls)))
  routes submodules)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *restas-js-lib* (empty-lib "RESTAS"))

(add-to-lib *restas-js-lib*
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
    ))

    

        