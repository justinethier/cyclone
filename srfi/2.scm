;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Implementation of SRFI-2: and-let*
;;;
(define-syntax and-let*
  (syntax-rules ()
    ; Special case
    ((and-let* ())
     #t)
    ; No CLAWS, just body
    ((and-let* () body ...)
     (begin body ...))
    ; Special cases of below - CLAWS with no body
    ((and-let* ((var expr)) )
     (let ((var expr))
       (and var)))
    ((and-let* ((expr)))
     (let ((tmp expr))
       (and tmp )))
    ((and-let* (expr))
     (let ((tmp expr))
       (and tmp )))
    ; General case - CLAWS and body
    ((and-let* ((var expr) . rest) . body)
     (let ((var expr))
       (and var (and-let* rest . body))))
    ((and-let* ((expr) . rest) . body)
     (let ((tmp expr))
       (and tmp (and-let* rest . body))))
    ((and-let* (expr . rest) . body)
     (let ((tmp expr))
       (and tmp (and-let* rest . body))))))

