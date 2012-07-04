(defpackage :docbrowser
  (:use :cl)
  (:export :start-docserver)
  (:documentation "Runs a webserver that allows the user to browse and search
information about the packages in the Lisp runtime, including functions,
classes and variables."))

(in-package :docbrowser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
