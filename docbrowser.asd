(defpackage :docbrowser-system
  (:use :cl :asdf)
  (:documentation "private: ASDF system package for Docbrowser."))

(in-package :docbrowser-system)

(defsystem docbrowser
  :name "docbrowser"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Web-based Common Lisp documentation browser"
  :depends-on (:hunchentoot
               :bordeaux-threads
               :yacc
               :parse-number
               :babel
               :closer-mop
               :swank
               :cl-json
               :flexi-streams)
  :components ((:module template
                        :serial t
                        :components ((:file "template-package")
                                     (:file "util")
                                     (:file "template")
                                     (:file "parser")))
               (:module src
                        :serial t
                        :depends-on ("template")
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "server")
                                     (:file "docbrowser")
                                     (:file "search")))))
