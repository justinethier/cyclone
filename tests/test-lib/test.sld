(define-library (test-lib test)
  (import (scheme base))
  (export
    my-or)
  (begin
(define-syntax my-or
  (er-macro-transformer
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)) #f)
             ((null? (cddr expr)) (cadr expr))
             (else
              (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                    (list (rename 'if) (rename 'tmp)
                          (rename 'tmp)
                          (cons (rename 'or) (cddr expr)))))))))
  ))
