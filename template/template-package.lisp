(defpackage :template
  (:use :cl)
  (:export
   :parse-template
   :exec-template-file-to-string 
   :exec-template-file))

(in-package :template)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
