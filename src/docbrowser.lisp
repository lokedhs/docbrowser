(in-package :docbrowser)

(declaim #.*compile-decl*)

(define-handler-fn package-list-screen "/package_list"
  (with-hunchentoot-stream (out)
    (let ((packages (sort (mapcar #'(lambda (package)
                                      (package-name package))
                                  (list-all-packages))
                          #'string<)))
      (template:exec-template-file (concatenate 'string *files-base-dir* "template/packages.tmpl")
                                   `((:packages . ,packages))
                                   out))))
