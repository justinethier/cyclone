(import (scheme base) (scheme write))

;(call-with-values (lambda () (values 1 2)) (lambda (x y) (write `(,x ,y))))
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var val) ...) . body)
     (let () (define var val) ... . body))))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call-with-current-continuation
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call-with-current-continuation
              (lambda (handler-k)
                (guard-k
                 (lambda ()
                   (let ((var condition))      ; clauses may SET! var
                     (guard-aux (handler-k (lambda ()
                                             (raise-continuable condition)))
                                clause ...))))))))
          (lambda ()
            (let ((res (begin e1 e2 ...)))
              (guard-k (lambda () res)))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp (result temp) reraise)))
    ((guard-aux reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp (result temp) (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (or test (guard-aux reraise clause1 clause2 ...)))
    ((guard-aux reraise (test result1 result2 ...))
     (if test (begin result1 result2 ...) reraise))
    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

(write
  (letrec* ((x 1)) x))

(write
  (guard (condition
          ((assq 'a condition) => cdr)
          ((assq 'b condition)))
    (raise (list (cons 'a 42))))) ;=> 42

(write
  (guard (condition
          ((assq 'a condition) => cdr)
          ((assq 'b condition)))
    (raise (list (cons 'b 23))))) ;=> (b . 23)

