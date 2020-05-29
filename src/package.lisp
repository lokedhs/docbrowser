(defpackage :docbrowser
  (:use :cl)
  (:export :start-docserver :stop-docserver)
  (:documentation "Runs a webserver that allows the user to browse and search
information about the packages in the Lisp runtime, including functions,
classes and variables."))
