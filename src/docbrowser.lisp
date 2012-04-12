(in-package :docbrowser)

(declaim #.*compile-decl*)

(define-handler-fn main-screen "/"
  (hunchentoot:redirect "/package_list"))

(define-handler-fn package-list-screen "/package_list"
  (with-hunchentoot-stream (out)
    (let ((packages (sort (mapcar #'(lambda (package)
                                      (package-name package))
                                  (list-all-packages))
                          #'string<)))
      (template:exec-template-file (concatenate 'string *files-base-dir* "template/packages.tmpl")
                                   `((:packages . ,packages))
                                   out))))

(defun resolve-arglist (function)
  #+sbcl (sb-introspect:function-lambda-list function)
  #-sbcl '(arglist not available))

(defun load-function-info (symbol)
  (let ((function (symbol-function symbol)))
    (list (cons :name (string symbol))
          (cons :documentation (documentation symbol 'function))
          (cons :args (let ((*print-case* :downcase))
                        (format nil "~{~a~^ ~}" (resolve-arglist function)))))))

(defun load-variable-info (symbol)
  (list (cons :name (string symbol))
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (princ-to-string (symbol-value symbol))))))

(define-handler-fn show-package-screen "/show_package"
  (with-hunchentoot-stream (out)
    (let ((package (find-package (hunchentoot:parameter "id"))))
      (destructuring-bind (functions variables)
          (loop
             for s being each external-symbol in package
             when (fboundp s) collect (load-function-info s) into functions
             when (boundp s) collect (load-variable-info s) into variables
             finally (return (list functions variables)))
        (template:exec-template-file (concatenate 'string *files-base-dir* "template/show_package.tmpl")
                                     `((:name      . ,(package-name package))
                                       (:functions . ,(sort functions #'string<
                                                            :key #'(lambda (v) (cdr (assoc :name v)))))
                                       (:variables . ,(sort variables #'string<
                                                            :key #'(lambda (v) (cdr (assoc :name v))))))
                                     out)))))
