;;;; restas.asd
;;;;
;;;; This file is part of the RESTAS-JS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-js
  :depends-on (#:restas #:cl-js)
  :components ((:file "restas-js")))
