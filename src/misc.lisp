(in-package :docbrowser)

(declaim #.*compile-decl*)

(defun assoc-cdr (key data &key error-p)
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
