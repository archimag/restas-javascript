;;;; utils.lisp
;;;;
;;;; This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.javascript)

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

