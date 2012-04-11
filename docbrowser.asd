(defpackage :docbrowser-system
  (:use :cl :asdf))

(in-package :docbrowser-system)

(defsystem docbrowser
  :name "docbrowser"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Documentation browser for SBCL"
  :depends-on (:hunchentoot
               :bordeaux-threads
               :yacc
               :parse-number
               :babel)
  :components ((:module template
                        :serial t
                        :components ((:file "template-package")
                                     (:file "util")
                                     (:file "template")
                                     (:file "parser")))
               (:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "server")
                                     (:file "docbrowser")))))
