(in-package :docbrowser)

(declaim #.*compile-decl*)

(defun format-source (source-descriptor)
  (let ((file (cadr (assoc :file source-descriptor)))
        (position (cadr (assoc :position source-descriptor))))
    (with-open-file (in file)
      (let ((position-line nil))
        (let ((result (with-output-to-string (out)
                        (loop
                           with current-position = 0
                           for line-number from 1
                           for line = (read-line in nil nil)
                           while line
                           do (incf current-position (length line))
                           when (and (not position-line) (< position current-position))
                           do (setq position-line line-number)
                           do (format out "<div class=\"code-line\"><span class=\"number-col\"><a name=\"l~a\"></a><a href=\"#l~a\">~a</a></span><span class=\"code-col\">~a</span></div></div>~%"
                                      line-number line-number line-number
                                      (if (zerop (length (string-trim " " line)))
                                          "&nbsp;"
                                          (with-output-to-string (s)
                                            (docbrowser-template:escape-string-minimal line s))))))))
          (values result position-line))))))

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
