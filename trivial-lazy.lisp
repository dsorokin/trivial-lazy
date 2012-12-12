;;;; TRIVIAL-LAZY -- Trivial lazy macros and functions for Common Lisp
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :trivial-lazy
  (:use :cl :bordeaux-threads)
  (:nicknames :lazy)
  (:export #:memo
           #:delay
           #:force))

(in-package :trivial-lazy)

(defparameter *memo-lock* (make-lock "MEMO"))

(defun memo (function &key (thread-safe nil))
  "Memoize the specified function."
  (let ((x-defined nil)
        (x nil))
    (lambda ()
      (cond
       (x-defined x)
       ((not thread-safe)
        (let ((x-next (funcall function)))
          (setf x x-next
                x-defined t)
          x-next))
       (t
        (with-lock-held (*memo-lock*)
          (if x-defined
              x
            (let ((x-next (funcall function)))
              (setf x x-next
                    x-defined t)
              x-next))))))))

(defmacro delay (exp &key (thread-safe nil))
  "Delay the expression."
  `(memo #'(lambda() ,exp) :thread-safe ,thread-safe))

(defun force (delayed-exp)
  "Force to return the value of the delayed expression."
  (funcall delayed-exp))