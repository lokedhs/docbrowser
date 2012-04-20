(in-package :docbrowser)

(declaim #.*compile-decl*)

#+sbcl(defmethod documentation ((slot sb-pcl::effective-slot-definition) (type t))
        "SBCL doesn't have a proper implementation of DOCUMENTATION for instances of
EFFECTIVE-SLOT-DEFINITION, but instead prints a warning when an attempt is make to
access it. This method will suppress this warning."
        nil)

(define-handler-fn main-screen "/"
  (hunchentoot:redirect "/package_list"))

(define-handler-fn package-list-screen "/package_list"
  (with-hunchentoot-stream (out)
    (let ((packages (sort (mapcar #'(lambda (package)
                                      (list (cons :name (package-name package))
                                            (cons :documentation (documentation package t))))
                                  (list-all-packages))
                          #'string< :key #'(lambda (v) (cdr (assoc :name v))))))
      (docbrowser-template:exec-template-file (concatenate 'string *files-base-dir* "template/packages.tmpl")
                                              `((:packages . ,packages))
                                              out))))

(defun load-function-info (symbol)
  (list (cons :name (string symbol))
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase))
                      (format nil "~{~a~^ ~}" (mapcar #'nice-princ-to-string (swank-backend:arglist symbol)))))
        (cons :type (cond ((macro-function symbol) "macro")
                          (t "function")))))

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

(defun find-method-info (symbol)
  (list (cons :name (princ-to-string symbol))))

(defun safe-class-for-symbol (symbol)
  (handler-case
      (find-class symbol)
    (error nil)))

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun specialise->symbol (spec)
  (case (caar spec)
    ((defmethod) (cadar spec))
    #+ccl((ccl::reader-method) (cadr (assoc :method (cdar spec))))
    (t nil)))

(defun load-specialisation-info (class-name)
  (let ((ignored '(initialize-instance))
        (class (if (symbolp class-name) (find-class class-name) class-name)))
    (sort (loop
             for v in (swank-backend:who-specializes class)
             for symbol = (specialise->symbol v)
             when (and (not (member symbol ignored))
                       (symbol-external-p symbol (symbol-package (class-name class))))
             collect (find-method-info symbol))
          #'string< :key #'assoc-name)))

(defun load-slots (class)
  (closer-mop:ensure-finalized class)
  (flet ((load-slot (slot)
           (list (cons :name (closer-mop:slot-definition-name slot))
                 (cons :documentation (swank-mop:slot-definition-documentation slot)))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun load-class-info (class-name)
  (let ((cl (find-class class-name)))
    (list (cons :name (class-name cl))
          (cons :documentation (documentation cl 'type))
          (cons :slots (load-slots cl))
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
        (docbrowser-template:exec-template-file (concatenate 'string *files-base-dir* "template/show_package.tmpl")
                                                `((:name      . ,(package-name package))
                                                  (:functions . ,(sort functions #'string< :key #'assoc-name))
                                                  (:variables . ,(sort variables #'string< :key #'assoc-name))
                                                  (:classes   . ,(sort classes #'string< :key #'assoc-name)))
                                                out)))))
