(in-package :docbrowser)

(declaim #.*compile-decl*)

(define-handler-fn package-list-screen "/package_list"
  (with-hunchentoot-stream (out)
    (template:parse-template (concatenate 'string *files-base-dir* "template/packages.tmpl") out)))
