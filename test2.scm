;(import (scheme eval) (scheme write))
;
;(eval '((lambda (expr rename compare) (cond ((null? (cdr expr)) #t) ((null? (cddr expr)) (cadr expr)) (else (list (rename (quote if)) (cadr expr) (cons (rename (quote and)) (cddr expr)) #f)))) '(test 1 2 3) (lambda (x) x) '()))
;;;(eval '((lambda (x) x) 1))
;
;
(import (scheme base)
        (scheme eval)
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
       (cond ((null? (cdr expr)) #t)
;       (cond ((null? (cdr expr)))
             ((null? (cddr expr)) (cadr expr))
             (else (list (rename 'if) (cadr expr)
                         (cons (rename 'and) (cddr expr))
                         #f))))))

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

(write (test 1 2 3))
(write (or 1 2 3 'or))
(write (or #f 2 3 'or))
;(test 'done)
'done

(define x 1)
(write x)
(write
  (eval 'x))
