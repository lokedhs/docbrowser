(defpackage :docbrowser
  (:use :cl)
  (:export
   :start-docserver))

(in-package :docbrowser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
