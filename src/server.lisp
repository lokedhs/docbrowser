(in-package :docbrowser)

(defparameter *files-base-dir*
  (format nil "~asrc/" (namestring (asdf:component-pathname (asdf:find-system :docbrowser)))))

(defclass docbrowser-acceptor (hunchentoot:acceptor)
  ((files-dispatcher :reader docbrowser-acceptor-files-dispatcher
                     :documentation "List of fallback dispatchers"))
  (:documentation "Acceptor for the documentation browser"))

(defmethod initialize-instance :after ((acceptor docbrowser-acceptor) &key &allow-other-keys)
  (setf (slot-value acceptor 'files-dispatcher)
        (mapcar #'(lambda (path)
                    (hunchentoot:create-folder-dispatcher-and-handler (format nil "/~a/" path)
                                                                      (pathname (concatenate 'string
                                                                                             *files-base-dir*
                                                                                             "files/" path "/"))))
                '("css" "js" "jquery-multi-open-accordion"))))

(defvar *url-handlers* (make-hash-table :test 'equal)
  "A hash table keyed on the base URL that maps to the underlying handler function")

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor docbrowser-acceptor) request)
  (let ((handler (gethash (hunchentoot:script-name request) *url-handlers*)))
    (if handler
        (funcall handler)
        (loop
           for dispatcher in (docbrowser-acceptor-files-dispatcher acceptor)
           for dis = (funcall dispatcher request)
           when dis
           do (progn
                (funcall dis)
                (return nil))
           finally (call-next-method)))))

(defun %make-define-handler-fn-form (docstring name body)
  `(defun ,name ()
     ,@(when docstring (list docstring))
     ,@body))

(defmacro define-handler-fn (name url &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,url *url-handlers*) ',name)
     ,(if (stringp (car body))
          (%make-define-handler-fn-form (car body) name (cdr body))
          (%make-define-handler-fn-form nil name body))))

(defun hunchentoot-stream-as-text (&key (content-type "text/html") (append-charset t))
  "Sends the appropriate headers to ensure that all data is sent back using
the correct encoding and returns a text stream that the result can be
written to."
  (when content-type
    (setf (hunchentoot:content-type*)
          (if append-charset
              (format nil "~a;charset=UTF-8" content-type)
              content-type)))
  (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf8))

(defmacro with-hunchentoot-stream ((out &optional (content-type "text/html") (append-charset t)) &body body)
  `(let ((,out (hunchentoot-stream-as-text :content-type ,content-type :append-charset ,append-charset)))
     ,@body))

(defun show-template (out file data)
  (docbrowser-template:exec-template-file (concatenate 'string *files-base-dir*
                                                       "template/" file)
                                          data out
                                          :binary t
                                          :encoding :utf-8))

(defvar *global-acceptor* nil
  "The acceptor for the currently running server.")

(defun start-docserver (&key (port 8080) (address nil))
  "Start the documentation server with a HTTP listener on port PORT.
parameter ADDRESS restricts server listening to IP address"
  (when *global-acceptor*
    (error "Server is already running"))
  (let ((a (make-instance 'docbrowser-acceptor :port port :address address)))
    (hunchentoot:start a)
    (setq *global-acceptor* a))
  (setq hunchentoot:*show-lisp-errors-p* t)
  (setq hunchentoot:*log-lisp-warnings-p* t)
  (setq hunchentoot:*log-lisp-backtraces-p* t)
  (setf (hunchentoot:acceptor-access-log-destination *global-acceptor*) (make-broadcast-stream))
  (format t "Docserver started on port ~a; listening to IP ~a" port (or address 'any))
  (values))

(defun stop-docserver ()
  "Stop the documentation server."
  (unless *global-acceptor*
    (error "Server is not running"))
  (hunchentoot:stop *global-acceptor*)
  (setf *global-acceptor* nil))
