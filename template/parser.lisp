(in-package :docbrowser)

(declaim #.*compile-decl*)

#+nil(cl-lex:define-string-lexer stream-template-lexer
       (""))

(defvar *begin-code* "<%")
(defvar *end-code* "%>")

(defun make-stream-template-lexer (input-stream)
  (let ((state :template)
        (current-line nil)
        (current-position 0))
    #'(lambda ()
        (flet ((parse-token ()
                 (let ((lexer-actions `(("^ +" ,(constantly :blank))
                                        ("^([a-z]+)" ,#'(lambda (exprs) (list 'symbol (aref exprs 0)))))))
                   (flet ((read-template ()
                            (let ((pos (search *begin-code* current-line :start2 current-position)))
                              (if (null pos)
                                  ;; no start code on this line, return the entire line
                                  (let ((result (subseq current-line current-position)))
                                    (setq current-line nil)
                                    (list 'template result))
                                  ;; start code found, return the prefix and switch state
                                  (let ((result (subseq current-line current-position pos)))
                                    (setq current-position (+ pos (length *begin-code*)))
                                    (setq state :code)
                                    (list 'template result))))))

                     (when (or (null current-line)
                               (> current-position (length current-line)))
                       (setq current-line (read-line input-stream nil nil)))
                     (if (null current-line)
                         ;; No more to read, simply return NIL
                         nil
                         ;; Else, check the current parser state
                         (ecase state
                           (:template (read-template))
                           (:code (loop
                                     for (regex action) in lexer-actions
                                     when (and (> (length current-line)
                                                  (+ current-position (length *end-code*)))
                                               (string= (subseq current-line current-position (+ current-position (length *end-code*)))
                                                        *end-code*))
                                     do (progn
                                          (incf current-position (length *end-code*))
                                          (setq state :template)
                                          :blank)
                                     do (multiple-value-bind (result exprs)
                                            (cl-ppcre:scan-to-strings regex current-line :start current-position)
                                          (when result
                                            (incf current-position (length result))
                                            (return (funcall action exprs))))
                                     finally (error "unmached string")))))))))
          (loop
             for token = (parse-token)
             if (null token) return nil
             unless (eq token :blank)
             return (apply #'values token))))))
