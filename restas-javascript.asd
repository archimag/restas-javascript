;;;; restas-javascript.asd
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-javascript
  :depends-on (#:restas #:cl-js)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "restas-javascript" :depends-on ("packages"))))))
