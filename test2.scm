(import (scheme base)
        (scheme write))

;(define-syntax test
;  (er-macro-transformer
;    (lambda (expr rename compare)
;      `((lambda ()
;        (write "testing")
;        (write (quote ,(cdr expr))))))))
;
; WTF is the macro unable to be evaluated when the same code works as part of *defined-macros*???
;
(define-syntax test 
  (er-macro-transformer
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)))
             ((null? (cddr expr)) (cadr expr))
             (else (list (rename 'if) (cadr expr)
                         (cons (rename 'and) (cddr expr))
                         #f))))))

(test 1 2 3)
;(test 'done)
'done
