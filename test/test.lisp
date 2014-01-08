(defpackage :trivial-lazy.test
  (:use :cl :trivial-lazy))

(in-package :trivial-lazy.test)

(defun lazy-1 ()
  (declare (optimize (debug 0)
                     (safety 0)
                     (speed 3)
                     (space 3)))
  (delay 1))

(defun lazy-1-opt ()
  (declare (optimize (debug 0)
                     (safety 0)
                     (speed 3)
                     (space 3)))
  ;(declare (inline memo))
  (delay 1 :thread-safe nil))

(defun lazy-1-opt-th ()
  (declare (optimize (debug 0)
                     (safety 0)
                     (speed 3)
                     (space 3)))
  (delay 1 :thread-safe t))

(disassemble #'lazy-1)
(disassemble #'lazy-1-opt)
(disassemble #'lazy-1-opt-th)
