;;;; TRIVIAL-LAZY -- Trivial lazy macros and functions for Common Lisp
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem :trivial-lazy
  :version "0.1"
  :description "Trivial lazy macros and functions for Common Lisp."
  :licence "MIT"
  :depends-on (bordeaux-threads)
  :components ((:file "trivial-lazy")
               (:static-file "README")))

