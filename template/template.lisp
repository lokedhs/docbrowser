(in-package :template)

(declaim #.*compile-decl*)

(defclass parsed-file ()
  ((name            :type pathname
                    :reader parsed-file-pathspec
                    :initarg :name
                    :initform (error "name is a required parameter"))
   (modified-date   :type integer
                    :reader parsed-file-modified-date)
   (last-time-check :type integer
                    :accessor parsed-file-last-time-check)
   (template        :type function
                    :reader parsed-file-template
                    :initarg :template
                    :initform (error "template is unset")))
  (:documentation "Cached parse result"))

(defmethod initialize-instance :after ((obj parsed-file) &key &allow-other-keys)
  (setf (slot-value obj 'modified-date) (file-write-date (parsed-file-pathspec obj)))
  (setf (slot-value obj 'last-time-check) (get-universal-time)))

(defvar *cached-templates* (make-hash-table :test 'equal))
(defvar *cached-templates-lock* (bordeaux-threads:make-lock "cached-templates-lock"))

(defun parse-template-file (pathname)
  (with-open-file (s pathname)
    (parse-template-by-stream s :binary t)))

(defun parse-template (file data stream)
  (bordeaux-threads:with-lock-held (*cached-templates-lock*)
    (let* ((pathname (pathname file))
           (cached (gethash pathname *cached-templates*)))
      (funcall (parsed-file-template (cond ((or (null cached)
                                                (> (file-write-date (parsed-file-pathspec cached))
                                                   (parsed-file-modified-date cached)))
                                            (setf (gethash pathname *cached-templates*)
                                                  (make-instance 'parsed-file
                                                                 :name pathname
                                                                 :template (parse-template-file pathname))))
                                           (t
                                            (setf (parsed-file-last-time-check cached) (get-universal-time))
                                            cached)))
               data stream))))

(defun parse-template-to-string (file data)
  (with-output-to-string (s)
    (parse-template file data s)))
