(in-package :template)

(declaim #.*compile-decl*)

(defvar *begin-code* "<%")
(defvar *end-code* "%>")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-lexer-actions-list (definitions)
    (mapcar #'(lambda (definition)
                (destructuring-bind (regex action)
                    definition
                  (list (cl-ppcre:create-scanner (concatenate 'string "^" regex))
                        (etypecase action
                          (symbol (constantly (list action nil)))
                          (function action)))))
            definitions))

  (defmacro make-lexer-actions (&rest definitions)
    `(make-lexer-actions-list (list ,@(mapcar #'(lambda (definition)
                                                  `(list ,(car definition) ,(cadr definition)))
                                              definitions))))  
) ; EVAL-WHEN

(defparameter *actions*
  (make-lexer-actions ("[ \\n]+" (constantly :blank))
                      ("if"     'if)
                      ("end"    'end)
                      ("while"  'while)
                      ("repeat" 'repeat)
                      ("for"    'for)
                      ("with"   'with)
                      (","      '|,|)
                      ("="      '|=|)
                      ("\\("    '|(|)
                      ("\\)"    '|(|)
                      ("@"      '|@|)
                      ("#"      '|#|)
                      ("\\."    '|.|)
                      ("([a-zA-Z_][a-zA-Z_0-9-]*)" (lambda (exprs) (list 'symbol (aref exprs 0))))
                      ("\"([^\"]*)\"" (lambda (exprs) (list 'string (aref exprs 0))))
                      ("([0-9]+)" (lambda (exprs) (list 'number (parse-number:parse-number (aref exprs 0)))))))

(defun make-stream-template-lexer (input-stream)
  (let ((lexer-actions *actions*)
        (state :template)
        (current-line nil)
        (current-position 0)
        (input-finish nil))

    #'(lambda ()
        (labels ((read-template ()
                   (let ((pos (search *begin-code* current-line :start2 current-position)))
                     (if (null pos)
                         ;; no start code on this line, return the entire line
                         (let ((result (subseq current-line current-position)))
                           (setq current-position (length current-line))
                           (list 'template result))
                         ;; start code found, return the prefix and switch state
                         (let ((result (subseq current-line current-position pos)))
                           (setq current-position (+ pos (length *begin-code*)))
                           (setq state :code)
                           (list 'template result)))))

                 (read-code ()
                   (if (and (>= (length current-line)
                                (+ current-position (length *end-code*)))
                            (string= current-line *end-code*
                                     :start1 current-position
                                     :end1 (+ current-position (length *end-code*))))
                       ;; End code was found
                       (progn
                         (incf current-position (length *end-code*))
                         (setq state :template)
                         :blank)
                       ;; No end code found, check the actions
                       (loop
                          with longest-match-length = 0
                          with longest-match-exprs = nil
                          with longest-match-action = nil
                          for (regex action) in lexer-actions                      
                          do (multiple-value-bind (result exprs)
                                 (cl-ppcre:scan-to-strings regex current-line :start current-position)
                               (when (and result
                                          (> (length result) longest-match-length))
                                 (setq longest-match-length (length result))
                                 (setq longest-match-exprs exprs)
                                 (setq longest-match-action action)))
                          finally (cond ((plusp longest-match-length)
                                         (incf current-position longest-match-length)
                                         (return (funcall longest-match-action longest-match-exprs)))
                                        (t
                                         (error "unmached string: ~s" (subseq current-line current-position)))))))

                 (read-next-line ()
                   (unless input-finish
                     (setq current-line (read-line input-stream nil nil))
                     (setq current-position 0)
                     (cond (current-line
                            :blank)
                           (t
                            (setq input-finish t)
                            nil))))

                 (parse-token ()
                   (cond ((null current-line)
                          (read-next-line))
                         ((>= current-position (length current-line))
                          (if (eq state :template)
                              ;; If processing the template part, return a newline
                              (progn
                                (read-next-line)
                                (list 'template (string #\Newline)))
                              ;; Else, simply process the next line
                              (read-next-line)))
                         (t
                          (ecase state
                            (:template (read-template))
                            (:code (read-code)))))))

          (loop
             for token = (parse-token)
             while token
             unless (eq token :blank)
             return (apply #'values token))))))

(defun debug-lexer (string)
  (with-input-from-string (s string)
    (let ((lex (make-stream-template-lexer s)))
      (loop
         for (a b) = (multiple-value-list (funcall lex))
         while a
         do (format t "~s ~s~%" a b)))))

(defun string->symbol (symbol-name &optional (package "KEYWORD"))
  (intern (string-upcase (string symbol-name)) package))

(defvar *current-content* nil)

(yacc:define-parser *template-parser*
  (:start-symbol document)
  (:terminals (template symbol string if end while repeat number for with |,| |=| |(| |)| |@| |#| |.|))
  (:precedence ((:right template)))

  (document
   (document-nodes #'(lambda (doc) `(progn ,@doc (values)))))

  (document-nodes
   (document-node document-nodes #'(lambda (n rest) (if n (append (list n) rest) rest)))
   nil)

  (template-list
   (template template-list #'(lambda (v1 v2) (concatenate 'string v1 v2)))
   template)

  (document-node
   (template-list #'(lambda (v) (when (plusp (length v)) `(princ ,v stream))))

   (if expression document-nodes end
       #'(lambda (v1 expr document v4)
           (declare (ignore v1 v4))
           `(if ,expr (progn ,@document))))

   (while expression document-nodes end
          #'(lambda (v1 expr document v4)
              (declare (ignore v1 v4))
              `(loop while ,expr do (progn ,@document))))

   (repeat number-expr document-nodes end
           #'(lambda (v1 number document v4)
               (declare (ignore v1 v4))
               `(loop repeat ,number do (progn ,@document))))

   (for data optional-variable-assignment document-nodes end
        #'(lambda (v1 data var-name document v5)
            (declare (ignore v1 v5))
            (let ((sym (if var-name
                        (string->symbol var-name "CL-USER")
                        (gensym))))
              `(loop
                  for ,sym in ,data
                  do (let ((*current-content* ,sym)) ,@document)))))

   (|#| data
        #'(lambda (v1 data)
            (declare (ignore v1))
            `(princ ,data stream)))

   )

  (optional-variable-assignment
   (with symbol #'(lambda (v1 symbol) (declare (ignore v1)) symbol))
   nil)

  (data
   (symbol #'(lambda (symbol-name)
               `(cdr (assoc ,(string->symbol symbol-name) *current-content*))))
   (|.| #'(lambda (v1) (declare (ignore v1)) '*current-content*)))

  (expression
   (symbol #'(lambda (v) `(cdr (assoc  ,v *current-content*)))))

  (number-expr
   (number #'identity))
  )

(defun parse-stream-to-form (stream)
  (yacc:parse-with-lexer (make-stream-template-lexer stream) *template-parser*))

(defun parse-template-by-stream (stream)
  (let* ((template-form (parse-stream-to-form stream))
         (name (gensym)))
    (compile name `(lambda (data stream)
                     (declare (ignorable data stream))
                     (let ((*current-content* data))
                       ,template-form)))
    (symbol-function name)))
