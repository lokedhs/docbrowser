(in-package :docbrowser)

(declaim #.*compile-decl*)

(define-handler-fn main-screen "/"
  (hunchentoot:redirect "/package_list"))

(define-handler-fn package-list-screen "/package_list"
  (with-hunchentoot-stream (out)
    (let ((packages (sort (mapcar #'(lambda (package)
                                      (list (cons :name (package-name package))
                                            (cons :documentation (documentation package t))))
                                  (list-all-packages))
                          #'string< :key #'(lambda (v) (cdr (assoc :name v))))))
      (show-template out "packages.tmpl" `((:packages . ,packages))))))

(defun format-argument-to-string (arg)
  (etypecase arg
    (symbol (nice-princ-to-string arg))
    (list   (mapcar #'(lambda (entry conversion) (funcall conversion entry))
                    arg (list #'(lambda (v)
                                  (if (listp v)
                                      (nice-princ-to-string (car v))
                                      (nice-princ-to-string v)))
                              #'prin1-to-string
                              #'nice-princ-to-string)))))

(defun load-function-info (symbol)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*package* (symbol-package symbol)))
                      (format nil "~{~a~^ ~}" (mapcar #'format-argument-to-string (swank-backend:arglist symbol)))))
        (cons :type (cond ((macro-function symbol) "macro")
                          ((typep (symbol-function symbol) 'generic-function) "generic function")
                          (t "function")))))

(defun load-variable-info (symbol)
  (list (cons :name (string symbol))
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (prin1-to-string (symbol-value symbol))))
        (cons :constant-p (constantp symbol))))

(defun find-superclasses (class)
  (labels ((f (classes found)
             (if (and classes
                      (not (eq (car classes) (find-class 'standard-object)))
                      (not (member (car classes) found)))
                 (f (cdr classes)
                    (f (closer-mop:class-direct-superclasses (car classes))
                       (cons (car classes) found)))
                 found)))
    (f (list class) nil)))

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
  (let* ((ignored '(initialize-instance))
         (class (if (symbolp class-name) (find-class class-name) class-name))
         (spec (swank-backend:who-specializes class)))
    (unless (eq spec :not-implemented)
      (sort (loop
               for v in spec
               for symbol = (specialise->symbol v)
               when (and (not (member symbol ignored))
                         (symbol-external-p symbol (symbol-package (class-name class))))
               collect (find-method-info symbol))
            #'string< :key #'assoc-name))))

(defun %ensure-external (symbol)
  (let ((name (cond ((symbolp symbol)
                     symbol)
                    ((and (listp symbol) (eq (car symbol) 'setf))
                     (cadr symbol))
                    (t
                     (warn "Unknown type: ~s. Expected symbol or SETF form." symbol)
                     nil))))
    (when (symbol-external-p name)
      symbol)))

(defun load-accessor-info (class slot)
  (flet ((getmethod (readerp method-list)
           (dolist (method method-list)
             (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method))))
               (when (and (eq (type-of method) (if readerp
                                                   'closer-mop:standard-reader-method
                                                   'closer-mop:standard-writer-method))
                          (eq (closer-mop:slot-definition-name (closer-mop:accessor-method-slot-definition method))
                              (closer-mop:slot-definition-name slot)))
                 (return-from getmethod name))))))

    ;; There are several different situations we want to detect:
    ;;   1) Only a reader method: "reader FOO"
    ;;   2) Only a writer method: "writer FOO"
    ;;   3) Only a writer SETF method: "writer (SETF FOO)"
    ;;   4) A reader and a SETF method: "accessor FOO"
    ;;   5) A reader and non-SETF writer: "reader FOO, writer FOO"
    ;;
    ;; The return value from this function is an alist of the following form:
    ;;
    ;;  ((:READER . FOO-READER) (:WRITER . FOO-WRITER) (:ACCESSOR . FOO-ACCESSOR))
    ;;
    ;; Note that if :ACCESSOR is given, then it's guaranteed that neither
    ;; :READER nor :WRITER will be included.
    ;;
    ;; We start by assigning the reader and writer methods to variables
    (let* ((method-list (closer-mop:specializer-direct-methods class))
           (reader (%ensure-external (getmethod t method-list)))
           (writer (%ensure-external (getmethod nil method-list))))
      ;; Now, detect the 5 different cases, but we coalease case 2 and 3.
      (cond ((and reader (null writer))
             `((:reader . ,reader)))
            ((and (null reader) writer)
             `((:writer . ,writer)))
            ((and reader (listp writer) (eq (car writer) 'setf) (eq (cadr writer) reader))
             `((:accessor . ,reader)))
            ((and reader writer)
             `((:reader . ,reader) (:writer . ,writer)))))))

(defun load-slots (class)
  (closer-mop:ensure-finalized class)
  (flet ((load-slot (slot)
           (list (cons :name (string (closer-mop:slot-definition-name slot)))
                 (cons :documentation (swank-mop:slot-definition-documentation slot))
                 ;; The LIST call below is because the accessor lookup is wrapped
                 ;; in a FOR statement in the template.
                 (cons :accessors (let ((accessor-list (load-accessor-info class slot)))
                                    (when accessor-list
                                      (list accessor-list)))))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun load-class-info (class-name)
  (let ((cl (find-class class-name)))
    (list (cons :name          (class-name cl))
          (cons :documentation (documentation cl 'type))
          (cons :slots         (load-slots cl))
          (cons :methods       (load-specialisation-info cl)))))

(defun %annotate-function-info (fn-info classes)
  "Append :ACCESSORP tag if the function is present as an accessor function."
  (loop
     with name = (cdr (assoc :name fn-info))
     for class-info in classes
     do (loop
           for slot-info in (cdr (assoc :slots class-info))
           do (loop
                 for accessor in (cdr (assoc :accessors slot-info))
                 for accessor-sym = (cdar accessor)
                 when (or (and (symbolp accessor-sym) (eq accessor-sym name))
                          (and (listp accessor-sym) (eq (car accessor-sym) 'setf) (eq (cadr accessor-sym) name)))
                 do (return-from %annotate-function-info (append fn-info '((:accessorp t))))))
     finally (return fn-info)))

(defun %show-package-screen-show (out package)
  (let ((*package* package))
    (destructuring-bind (functions variables classes)
        (loop
           for s in (loop
                       for symbol being each external-symbol in package
                       when (eq (symbol-package symbol) package)
                       collect symbol)
           when (safe-class-for-symbol s) collect (load-class-info s) into classes
           when (fboundp s) collect (load-function-info s) into functions
           when (boundp s) collect (load-variable-info s) into variables
           finally (return (list (mapcar #'(lambda (fn)
                                             (%annotate-function-info fn classes))
                                         functions)
                                 variables classes)))
      (show-template out "show_package.tmpl" `((:name      . ,(package-name package))
                                               (:functions . ,(sort functions #'string< :key #'assoc-name))
                                               (:variables . ,(sort variables #'string< :key #'assoc-name))
                                               (:classes   . ,(sort classes #'string< :key #'assoc-name)))))))

(define-handler-fn show-package-screen "/show_package"
  (with-hunchentoot-stream (out)
    (let* ((package-name (hunchentoot:parameter "id"))
           (package (find-package package-name)))
      (if (null package)
          ;; If the package does not exist, display the error page
          (show-template out "package_does_not_exist.tmpl" `((:name . ,package-name)))
          ;; The package does exist, display it
          (%show-package-screen-show out package)))))
