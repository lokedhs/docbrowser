(defpackage :docbrowser-template
  (:use :cl)
  (:export
   :parse-template
   :exec-template-file-to-string 
   :exec-template-file
   :template-error))

(in-package :docbrowser-template)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
