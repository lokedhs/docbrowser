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

(defun load-function-info (symbol)
  (let ((function (symbol-function symbol)))
    (list (cons :name (string symbol))
          (cons :documentation (documentation symbol 'function))
          (cons :args (let ((*print-case* :downcase))
                        (format nil "~{~a~^ ~}" (swank-backend:arglist function)))))))

(defun load-variable-info (symbol)
  (list (cons :name (string symbol))
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (princ-to-string (symbol-value symbol))))))

(defun find-superclasses (class)
  (append (loop
             for parent-class in (closer-mop:class-direct-superclasses class)
             append (load-specialisation-info parent-class))
          (list (class-name class))))

(defun find-method-info (method)
  (let ((symbol (cadar method)))
    (list (cons :name (princ-to-string symbol)))))

(defun safe-class-for-symbol (symbol)
  (handler-case
      (find-class symbol)
    (error nil)))

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun load-specialisation-info (class)
  (let ((ignored '(initialize-instance))
        (v (if (symbolp class) (find-class class) class)))
    (sort (loop
             for v in (swank-backend:who-specializes v)
             for symbol = (cadar v)
             when (and (not (member symbol ignored))
                       (symbol-external-p symbol (symbol-package (class-name class))))
             collect (find-method-info v))
          #'string< :key #'assoc-name)))

(defun load-class-info (class-name)
  (let ((cl (find-class class-name)))
    (list (cons :name (class-name cl))
          (cons :documentation (documentation cl 'type))
          (cons :methods (load-specialisation-info cl)))))

(define-handler-fn show-package-screen "/show_package"
  (with-hunchentoot-stream (out)
    (let ((package (find-package (hunchentoot:parameter "id"))))
      (destructuring-bind (functions variables classes)
          (loop
             for s being each external-symbol in package
             when (fboundp s) collect (load-function-info s) into functions
             when (boundp s) collect (load-variable-info s) into variables
             when (safe-class-for-symbol s) collect (load-class-info s) into classes
             finally (return (list functions variables classes)))
        (template:exec-template-file (concatenate 'string *files-base-dir* "template/show_package.tmpl")
                                     `((:name      . ,(package-name package))
                                       (:functions . ,(sort functions #'string< :key #'assoc-name))
                                       (:variables . ,(sort variables #'string< :key #'assoc-name))
                                       (:classes   . ,(sort classes #'string< :key #'assoc-name)))
                                     out)))))
