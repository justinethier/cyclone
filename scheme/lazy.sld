(define-library (scheme lazy)
  (import (scheme base))
  (export 
    delay 
    force 
    delay-force 
    make-promise 
    ;promise?
  )
  (begin
;    (define (make-promise x)
;      (delay x))
;  ;)
;  ;(begin
;    (define (promise? x)
;      (and (pair? x)
;           (null? (cdr x))
;           (pair? (car x))
;           (or (eq? #t (caar x))
;               (and (eq? #f (caar x))
;                    (procedure? (cdar x))))))
;
;  (define-syntax delay-force
;    (er-macro-transformer
;     (lambda (expr rename compare)
;       `(,(rename 'promise) #f (,(rename 'lambda) () ,(cadr expr))))))
;
;  (define-syntax delay
;    (er-macro-transformer
;     (lambda (expr rename compare)
;       `(,(rename 'delay-force) (,(rename 'promise) #t ,(cadr expr))))))
;
;  (define (promise done? proc)
;    (list (cons done? proc)))
;  (define (promise-done? x) (car (car x)))
;  (define (promise-value x) (cdr (car x)))
;  (define (promise-update! new old)
;    (set-car! (car old) (promise-done? new))
;    (set-cdr! (car old) (promise-value new))
;    (set-car! new (car old)))
;  (define (force promise)
;    (if (promise-done? promise)
;        (promise-value promise)
;        (let ((promise* ((promise-value promise))))
;          (if (not (promise-done? promise))
;            (promise-update! promise* promise))
;          (force promise))))))

  ;; Delayed evaluation functions from husk
  (define force
      (lambda (object)
        (object)))

  (define-syntax delay
    (er-macro-transformer
     (lambda (expr rename compare)
       `(make-promise (lambda () ,(cadr expr))))))
  (define-syntax delay-force
    (er-macro-transformer
     (lambda (expr rename compare)
       `(make-promise (lambda () ,(cadr expr))))))
  ;(define-syntax delay 
  ;  (syntax-rules () 
  ;    ((delay expression)
  ;     (make-promise (lambda () expression)))))

  (define make-promise
    (lambda (proc)
      (let ((result-ready? #f)
            (result #f))
        (lambda ()
          (if result-ready? 
              result
              (let ((x (proc)))
                (if result-ready?
                    result
                    (begin (set! result x)
                           (set! result-ready? #t)
                           result))))))))
))
