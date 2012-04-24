(in-package :docbrowser)

(declaim #.*compile-decl*)

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

(define-json-handler-fn search-command-screen "/search_command" data
  "Handles for AJAX-calls to perform a search operation"
  (hunchentoot:log-message* :info "Got data: ~s" data))
