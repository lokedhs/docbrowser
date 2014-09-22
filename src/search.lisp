(in-package :docbrowser)

(defvar *maximum-search-results* 100
  "The maximum number of search results returned.")

(define-handler-fn search-screen "/search"
  (with-hunchentoot-stream (out)
    (show-template out "search.tmpl" nil)))

(defun process-json (fn)
  (check-type fn function)
  (let* ((data (json:decode-json (hunchentoot:raw-post-data :want-stream t)))
         (result (funcall fn data)))
    (with-hunchentoot-stream (out "application/x-msgpack")
      (json:encode-json result out))))

(defmacro define-json-handler-fn (name url data-symbol &body body)
  (check-type data-symbol symbol)
  `(define-handler-fn ,name ,url
     ,@(when (stringp (car body)) (list (car body)))
     (process-json #'(lambda (,data-symbol) ,@(if (stringp (car body)) (cdr body) body)))))

(defun find-matching-symbols (string)
  (loop
     with normalised-string = (string-upcase string)
     with excluded = (mapcar #'find-package '("CL-USER" "KEYWORD"))
     for package in (list-all-packages)
     unless (find package excluded)
     append (loop
               for symbol being each external-symbol in package
               for symbol-as-string = (string-upcase (symbol-name symbol))
               when (and (search normalised-string symbol-as-string)
                         (or (safe-class-for-symbol symbol)
                             (fboundp symbol)
                             (boundp symbol)))
               collect symbol)))

(defun process-search-result (symbols)
  (flet ((function-info (symbol)
           (when (fboundp symbol)
             (list (list (cons :type :function)
                         (cons :name (prin1-to-string symbol))))))

         (variable-info (symbol)
           (when (boundp symbol)
             (list (list (cons :type :variable)
                         (cons :name (prin1-to-string symbol))))))

         (class-info (symbol)
           (when (safe-class-for-symbol symbol)
             (list (list (cons :type :class)
                         (cons :name (prin1-to-string symbol)))))))

    (loop
       for symbol in symbols
       repeat *maximum-search-results*
       append (append (function-info symbol)
                      (variable-info symbol)
                      (class-info symbol)))))

(define-json-handler-fn search-command-screen "/search_command" data
  "Handler for AJAX-calls to perform a search operation"
  (let ((search-text (string-trim " " (cdr (assoc :text data)))))
    (if (< (length search-text) 2)
        ;; The search text is too short
        nil
        ;; Search text acceptable, find all matching results
        (process-search-result (find-matching-symbols search-text)))))
