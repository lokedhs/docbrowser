(in-package :docbrowser)

(declaim #.*compile-decl*)

(defun read-file-content (file)
  (with-open-file (in file)
    (with-output-to-string (out)
      (loop
         with buf = (make-array 1024 :element-type 'character)
         for length = (read-sequence buf in)
         do (when (plusp length) (write-sequence buf out :end length))
         until (< length (array-dimension buf 0))))))

(defun style-from-type (type)
  (cond ((member :symbol type)  "lisp-symbol-class")
        ((member :string type)  "lisp-string-class")
        ((member :comment type) "lisp-comment-class")
        (t                      "lisp-normal-class")))

(defun escape-char (ch)
  (case ch
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (t   (string ch))))

(defun format-source (source-descriptor)
  (let* ((file (cadr (assoc :file source-descriptor)))
         (target-position (cadr (assoc :position source-descriptor))))
    (let ((target-found-row nil)
          (current-line 1)
          (current-style nil))
      (let ((code
             (with-output-to-string (out)
               (flet ((start-line ()
                        (format out "<tr><td><a name=\"~a\"></a><a href=\"#~a\">~a</a></td><td>"
                                current-line current-line current-line))

                      (end-line ()
                        (when current-style
                          (format out "</span>")
                          (setq current-style nil))
                        (format out "</td></tr>~%")))

                 (start-line)
                 (loop
                    with current-position = 0

                    for v in (colorize:scan-string :lisp (read-file-content file))
                    for style-type = (style-from-type (car v))

                    do (loop
                          for ch across (cdr v)
                          do (incf current-position)
                          when (and (not target-found-row) (> current-position target-position))
                          do (setf target-found-row current-line)
                          do (cond ((char= ch #\Newline)
                                    (incf current-line)
                                    (end-line)
                                    (start-line))
                                   (t
                                    (unless (equal style-type current-style)
                                      (when current-style
                                        (format out "</span>"))
                                      (setq current-style style-type)
                                      (format out "<span class=\"~a\">" current-style))
                                    (write-string (escape-char ch) out)))))
                 (end-line)))))
        (values code target-found-row)))))

(define-handler-fn source-function-screen "/source_function"
  (with-hunchentoot-stream (out)
    (let* ((p (find-package (hunchentoot:parameter "p")))
           (n (find-symbol (hunchentoot:parameter "n") p))
           (source-descriptor (swank-backend:find-source-location (symbol-function n))))
      (cond ((eq (car source-descriptor) :error)
             (format out "~a" (cadr source-descriptor)))
            ((eq (car source-descriptor) :location)
             (multiple-value-bind (code line)
                 (format-source (cdr source-descriptor))
               (show-template out "source_function.tmpl"
                              `((:name . ,(princ-to-string n))
                                (:line . ,line)
                                (:code . ,code)))))
            (t
             (error "Unexpected source descriptor: ~s" source-descriptor))))))
