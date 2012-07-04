(in-package :docbrowser)

(declaim #.*compile-decl*)

(defun format-source (source-descriptor)
  (let* ((file (cadr (assoc :file source-descriptor)))
         (target-position (cadr (assoc :position source-descriptor))))
    (loop
       with current-position = 0
       with current-line
       for v in (colorize:scan-string (read-file-content file))
         )))

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
