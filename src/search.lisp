(in-package :docbrowser)

(declaim #.*compile-decl*)

(define-handler-fn search-screen "/search"
  (with-hunchentoot-stream (out)
    (show-template out "search.tmpl" nil)))
