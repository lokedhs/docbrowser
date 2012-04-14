(in-package :docbrowser)

(declaim #.*compile-decl*)

(defun assoc-cdr (key data &key error-p)
  "Return (CDR (ASSOC KEY DATA)). If ERROR-P is non-NIL, signal an error if KEY is
not available is DATA."
  (let ((v (assoc key data)))
    (when (and error-p
               (not v))
      (error "~s not found in data" key))
    (cdr v)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "Return non-NIL if SYMBOL is external in PACKAGE. SYMBOL may be either
a symbol, or a SETF form, in which case the check will be performed on
the CADR of the list."
  (eq (nth-value 1 (find-symbol (symbol-name (cond ((symbolp symbol)
                                                    symbol)
                                                   ((eq (car symbol) 'setf)
                                                    (cadr symbol))
                                                   (t
                                                    (error "Unknown symbol type: ~s" symbol))))
                                package))
      :external))

(defun nice-princ-to-string (obj)
  (typecase obj
    (string obj)
    (keyword (prin1-to-string obj))
    (t (princ-to-string obj))))
