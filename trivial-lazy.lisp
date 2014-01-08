;;;; TRIVIAL-LAZY -- Trivial lazy macros and functions for Common Lisp
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :trivial-lazy
  (:use :cl :bordeaux-threads)
  (:nicknames :tlazy)
  (:export #:memo
           #:*memo-thread-safe*
           #:delay
           #:force
           #:thunk))

(in-package :trivial-lazy)

(deftype thunk (&optional result)
  `(function () ,result))

(defparameter *memo-lock* (make-lock "MEMO")
  "The global lock.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type boolean *memo-thread-safe*))
  (defparameter *memo-thread-safe* nil
    "Defines whether the memo is thread-safe."))

(declaim (ftype (function ((thunk) &key (:thread-safe boolean))
                          (values (thunk)))
                memo))
(declaim (inline memo))
(defun memo (function &key (thread-safe *memo-thread-safe*))
  "Memoize the specified function."
  (declare (optimize (debug 0)
                     (safety 0)
                     (speed 3)
                     (space 3)))
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

(defmacro delay (exp &key (thread-safe *memo-thread-safe* thread-safe-p))
  "Delay the expression."
  (if thread-safe-p
      `(memo (lambda () ,exp) :thread-safe ,thread-safe)
    `(memo (lambda () ,exp))))

(declaim (inline force))
(defun force (delayed-exp)
  "Force to return the value of the delayed expression."
  (funcall delayed-exp))
