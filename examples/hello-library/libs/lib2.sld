(define-library (libs lib2)
    (import (scheme base))
    (export lib2-hello or)
    (begin

(define-syntax or
  (er-macro-transformer
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)) #f)
             ((null? (cddr expr)) (cadr expr))
             (else
              (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                    (list (rename 'if) (rename 'tmp)
                          (rename 'tmp)
                          (cons (rename 'or) (cddr expr)))))))))

        (define lib2-hello 
            "Hello from library #2")))
