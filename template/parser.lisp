(in-package :template)

(declaim #.*compile-decl*)

(defvar *begin-code* "<%")
(defvar *end-code* "%>")

(defvar *current-line-num* nil
  "Dynamic variable used to track the current line number during parsing")

(defvar *output-binary* nil)
(defvar *output-encoding* nil)

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
                                         (error "Syntax error at line ~a:~a:~%~a~%~a^"
                                                *current-line-num* current-position
                                                current-line
                                                (with-output-to-string (s)
                                                  (loop
                                                     repeat current-position
                                                     do (princ " " s)))))))))

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
                                        (:terminals (template symbol string if end while repeat number for with
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

   ((if expression document-nodes end)
    `(if ,expression (progn ,@document-nodes)))

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
        `(princ (escape-string-minimal-plus-quotes ,data) stream)))

   )

  (optional-variable-assignment
   ((with symbol) symbol)
   nil)

  (data
   ((symbol)     `(cdr (assoc ,(string->symbol symbol "KEYWORD") *current-content*)))
   ((|.|)        '*current-content*)
   ((|,| symbol) (string->symbol symbol))
   ((string)     string))

  (expression
   ((symbol) `(cdr (assoc  ,symbol *current-content*))))

  (number-expr
   ((number) number))

)

(defun parse-stream-to-form (stream binary encoding)
  (let ((*package* (find-package :template-parse))
        (*current-line-num* 0)
        (*output-binary* binary)
        (*output-encoding* encoding))
    (yacc:parse-with-lexer (make-stream-template-lexer stream) *template-parser*)))

(defun parse-template-by-stream (stream &key binary (encoding :utf-8))
  (let* ((template-form  (parse-stream-to-form stream binary encoding))
         (name (gensym)))
    (compile name `(lambda (data stream)
                     (declare (ignorable data stream))
                     (let ((*current-content* data))
                       ,template-form)))
    (symbol-function name)))
