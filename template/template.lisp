(in-package :template)

(declaim #.*compile-decl*)

(defclass parsed-file ()
  ((name            :type string
                    :reader parsed-file-pathspec
                    :initarg :name
                    :initform (error "name is a required parameter"))
   (modified-date   :type integer
                    :reader parsed-file-modified-date
                    :initform (error "modified-date is unset"))
   (last-time-check :type integer
                    :initform (error "last-time-check is unset"))
   (template        :type function
                    :reader parsed-file-template
                    :initarg :template
                    :initform (error "template is unset")))
  (:documentation "Cached parse result"))

(defvar *cached-templates* (make-hash-table :test 'equal))
(defvar *cached-templates-lock* (bordeaux-threads:make-lock "cached-templates-lock"))

(defun parse-template-file (pathname)
  (with-open-file (s pathname)
    ))

(defun parse-template (file stream)
  (bordeaux-threads:with-lock-held (*cached-templates-lock*)
    (let* ((pathname (pathname file))
           (cached (gethash pathname *cached-templates*)))
      (funcall (parsed-file-template (if (or (null cached)
                                             (> (file-write-date (parsed-file-pathspec cached))
                                                (parsed-file-modified-date cached)))
                                         (setf (gethash pathname *cached-templates*) (parse-template-file pathname))
                                         cached))
               stream))))

(defun parse-template-to-string (file)
  (with-output-to-string (s)
    (parse-template file s)))
