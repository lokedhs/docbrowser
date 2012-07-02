(defpackage :docbrowser-template
  (:use :cl)
  (:export
   :parse-template
   :exec-template-file-to-string 
   :exec-template-file
   :template-error
   :template-error-line
   :template-error-column
   :template-error-message
   :template-error-content
   :template-error-content-index
   :*begin-code*
   :*end-code*
   :*current-content*
   :escape-string
   :escape-string-minimal
   :escape-string-minimal-plus-quotes
   :escape-string-all))

(in-package :docbrowser-template)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
