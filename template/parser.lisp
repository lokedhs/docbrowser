(in-package :template)

(declaim #.*compile-decl*)

(defvar *begin-code* "<%")
(defvar *end-code* "%>")

(defvar *current-line-num* nil
  "Dynamic variable used to track the current line number during parsing")

(defvar *output-binary* nil)
(defvar *output-encoding* nil)

(define-condition template-error (error)
  ((line          :type integer
                  :initarg :line
                  :initform (error "~s required when creating ~s" :line 'template-error)
                  :reader template-error-line
                  :documentation "The line number where the error occurred")
   (column        :type (or nil integer)
                  :initarg :column
                  :initform nil
                  :reader template-error-column
                  :documentation "The column index of the line where the error
occurred, if available. Otherwise NIL.")
   (message       :type string
                  :initarg :message
                  :initform (error "~s required when creating ~s" :message 'template-error)
                  :reader template-error-message
                  :documentation "The error message")
   (content       :type (or nil string)
                  :initarg :content
                  :initform nil
                  :reader template-error-content
                  :documentation "The actual template content where the error
occurred. Either the entire line, or part of it.")
   (content-index :type (or nil integer)
                  :initarg :content-index
                  :initform nil
                  :reader template-error-content-index
                  :documentation "The position in content closest to the actual error"))
  (:documentation "Error that is raised if there is an error parsing a template")
  (:report (lambda (condition stream)
             (with-slots (line column message content content-index) condition
               (format stream "Line ~a" line)
               (when column
                 (format stream ", column ~a" column))
               (format stream ": ~a" message)
               (when content
                 (format stream "~%~a~%~,,v@a" content content-index "^"))))))

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
                      ("else"   'else)
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
                      ("\"((?:(?:\\\\\")|[^\"])*)\"" (lambda (exprs)
                                                       (list 'string (escape-string-slashes (aref exprs 0)))))
                      ("([0-9]+)" (lambda (exprs) (list 'number (parse-number:parse-number (aref exprs 0)))))))

(defun signal-template-error (message &optional column content content-index)
  (error 'template-error
         :line *current-line-num*
         :column column
         :message message
         :content content
         :content-index content-index))

(defun escape-string-slashes (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
         for ch = (read-char in nil nil)
         for i from 0
         while ch
         do (cond ((char= ch #\\)
                   (case (prog1 (read-char in) (incf i))
                     ((#\") (write-char #\" out))
                     (t     (signal-template-error "Illegal escape sequence"
                                                   nil (format nil "\"~a\"" string) (1+ i)))))
                  (t
                   (write-char ch out)))))))

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
                   (cond ((and (>= (length current-line)
                                    (+ current-position (length *end-code*)))
                                (string= current-line *end-code*
                                         :start1 current-position
                                         :end1 (+ current-position (length *end-code*))))
                          ;; End code was found
                          (incf current-position (length *end-code*))
                          (setq state :template)
                          :blank)
                         (t
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
                                            (signal-template-error "Syntax error"
                                                                   current-position
                                                                   current-line
                                                                   current-position)))))))

                 (read-next-line ()
                   (unless input-finish
                     (setq current-line (read-line input-stream nil nil))
                     (incf *current-line-num*)
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

(defpackage :template-parse)

(defun string->symbol (symbol-name &optional (package *package*))
  (intern (string-upcase (string symbol-name)) package))

(defvar *current-content* nil)

(defmacro short-define-parser (name initials &body definitions)
  (labels ((process-row (row)
             (let* ((arguments (car row))
                    (param-list (mapcar #'(lambda (arg) (if (listp arg) (cadr arg) arg)) arguments)))
               (append (mapcar #'(lambda (arg) (if (listp arg) (car arg) arg)) arguments)
                       (when (cadr row) (list `#'(lambda ,param-list
                                                   (declare (ignorable ,@param-list))
                                                   ,@(cdr row)))))))

           (process-definition (definition)
             (append (list (car definition))
                     (mapcar #'process-row (cdr definition)))))

    `(yacc:define-parser ,name
       ,@initials
       ,@(mapcar #'process-definition definitions))))

(short-define-parser *template-parser* ((:start-symbol document)
                                        (:terminals (template symbol string if end else while repeat number for with
                                                              |,| |=| |(| |)| |@| |#| |.|))
                                        (:precedence ((:right template))))
                     
  (document
   ((document-nodes)
    `(progn ,@document-nodes (values))))

  (document-nodes
   ((document-node document-nodes)
    (if document-node
        (append (list document-node) document-nodes)
        document-nodes))
   (nil))

  (template-list
   ((template template-list)
    (concatenate 'string template template-list))
   ((template)
    template))

  (document-node
   ((template-list)
    (when (plusp (length template-list))
      (if *output-binary*
          `(write-sequence ,(babel:string-to-octets template-list :encoding *output-encoding*) stream)
          `(princ ,template-list stream))))

   ((if expression document-nodes else-statement end)
    `(if ,expression (progn ,@document-nodes) ,else-statement))

   ((while expression document-nodes end)
    `(loop while ,expression do (progn ,@document-nodes)))

   ((repeat number-expr (optional-variable-assignment var-name) document-nodes end)
    (let ((sym (if var-name
                   (string->symbol var-name)
                   (gensym))))
      `(loop for ,sym from 0 below ,number-expr do (progn ,@document-nodes))))

   ((for data (optional-variable-assignment var-name) document-nodes end)
    (let ((sym (if var-name
                   (string->symbol var-name)
                   (gensym))))
      `(loop
          for ,sym in ,data
          do (let ((*current-content* ,sym)) ,@document-nodes))))

   ((|#| data)
    (if *output-binary*
        `(write-sequence (babel:string-to-octets (escape-string-minimal-plus-quotes (princ-to-string ,data))
                                                 :encoding ,*output-encoding*)
                         stream)
        `(princ (escape-string-minimal-plus-quotes (princ-to-string ,data)) stream)))

   )

  (else-statement
   ((else document-nodes)
    `(progn ,@document-nodes))
   nil)

  (optional-variable-assignment
   ((with symbol) symbol)
   nil)

  (data
   ((symbol)     `(cdr (assoc ,(string->symbol symbol "KEYWORD") *current-content*)))
   ((|.|)        '*current-content*)
   ((|,| symbol) (string->symbol symbol))
   ((string)     string))

  (expression
   ((data) data #+nil`(cdr (assoc ,data *current-content*))))

  (number-expr
   ((number) number))

)

(defun parse-stream-to-form (stream binary encoding)
  (let ((*package* (find-package :template-parse))
        (*current-line-num* 0)
        (*output-binary* binary)
        (*output-encoding* encoding))
    (yacc:parse-with-lexer (make-stream-template-lexer stream) *template-parser*)))

(defun parse-template (stream &key binary (encoding :utf-8))
  "Parses and compiles the template defition given as STREAM. If BINARY
is NIL, the generated template will output its data as strings \(using
PRINC), otherwise the output will be converted to binary using the
encoding specified by ENCODING. The binary output is the preferred
method as that will allow constant strings in the template to be
encoded during parsing instead of at runtime.

The return value is a function that takes two arguments, DATA and OUTPUT.
DATA is the data that will be used by the template, and OUTPUT is the
output stream to which the result should be written."
  (let* ((template-form  (parse-stream-to-form stream binary encoding))
         (name (gensym)))
    (compile name `(lambda (data stream)
                     (declare (ignorable data stream))
                     (let ((*current-content* data))
                       ,template-form)))
    (symbol-function name)))
