(import (scheme base) (scheme write))

;(define-syntax let*-values
;  (syntax-rules ()
;    ((let*-values () . body)
;     (begin . body))
;    ((let*-values (((a) expr) . rest) . body)
;     (let ((a expr)) (let*-values rest . body)))
;    ((let*-values ((params expr) . rest) . body)
;     (call-with-values (lambda () expr)
;       (lambda params (let*-values rest . body))))))

(define-syntax my-let-values
  (syntax-rules ()
    ((my-let-values ("step") (binds ...) bind expr maps () () . body)
     (let*-values (binds ... (bind expr)) (let maps . body)))
    ((my-let-values ("step") (binds ...) bind old-expr maps () ((params expr) . rest) . body)
     (my-let-values ("step") (binds ... (bind old-expr)) () expr maps params rest . body))
    ((my-let-values ("step") binds (bind ...) expr (maps ...) (x . y) rest . body)
     (my-let-values ("step") binds (bind ... tmp) expr (maps ... (x tmp)) y rest . body))
    ((my-let-values ("step") binds (bind ...) expr (maps ...) x rest . body)
     (my-let-values ("step") binds (bind ... . tmp) expr (maps ... (x tmp)) () rest . body))
    ((my-let-values ((params expr) . rest) . body)
     (my-let-values ("step") () () expr () params rest . body))
    ))

;(list
;  (my-let-values))

(write
  (my-let-values (((a b c) (values 1 2 3))) (list a b c)))
