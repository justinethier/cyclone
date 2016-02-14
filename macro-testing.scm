(import (scheme base) (scheme write) (scheme case-lambda))

;(call-with-values (lambda () (values 1 2)) (lambda (x y) (write `(,x ,y))))
;(define-syntax letrec*
;  (syntax-rules ()
;    ((letrec* ((var val) ...) . body)
;     (let () (define var val) ... . body))))
;
;(define-syntax guard
;  (syntax-rules ()
;    ((guard (var clause ...) e1 e2 ...)
;     ((call-with-current-continuation
;       (lambda (guard-k)
;         (with-exception-handler
;          (lambda (condition)
;            ((call-with-current-continuation
;              (lambda (handler-k)
;                (guard-k
;                 (lambda ()
;                   (let ((var condition))      ; clauses may SET! var
;                     (guard-aux (handler-k (lambda ()
;                                             (raise-continuable condition)))
;                                clause ...))))))))
;          (lambda ()
;            (let ((res (begin e1 e2 ...)))
;              (guard-k (lambda () res)))))))))))
;
;(define-syntax guard-aux
;  (syntax-rules (else =>)
;    ((guard-aux reraise (else result1 result2 ...))
;     (begin result1 result2 ...))
;    ((guard-aux reraise (test => result))
;     (let ((temp test))
;       (if temp (result temp) reraise)))
;    ((guard-aux reraise (test => result) clause1 clause2 ...)
;     (let ((temp test))
;       (if temp (result temp) (guard-aux reraise clause1 clause2 ...))))
;    ((guard-aux reraise (test))
;     (or test reraise))
;    ((guard-aux reraise (test) clause1 clause2 ...)
;     (or test (guard-aux reraise clause1 clause2 ...)))
;    ((guard-aux reraise (test result1 result2 ...))
;     (if test (begin result1 result2 ...) reraise))
;    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
;     (if test
;         (begin result1 result2 ...)
;         (guard-aux reraise clause1 clause2 ...)))))
;
;   (define-syntax %case
;     (syntax-rules ()
;       ((%case args len n p ((params ...) . body) . rest)
;        (if (= len (length '(params ...)))
;            (apply (lambda (params ...) . body) args)
;            (%case args len 0 () . rest)))
;       ((%case args len n (p ...) ((x . y) . body) . rest)
;        (%case args len (+ n 1) (p ... x) (y . body) . rest))
;       ((%case args len n (p ...) (y . body) . rest)
;        (if (>= len n)
;            (apply (lambda (p ... . y) . body) args)
;            (%case args len 0 () . rest)))
;       ((%case args len n p)
;        (error "case-lambda: no cases matched"))))
;   (define-syntax case-lambda
;     (syntax-rules ()
;       ((case-lambda . clauses)
;        (lambda args (let ((len (length args))) (%case args len 0 () . clauses))))))
;        ;(lambda args (let ((len (length* args))) (%case args len 0 () . clauses))))))
;
;(define-syntax define-values
;  (syntax-rules ()
;    ((define-values () expr)
;     (define dummy
;       (call-with-values (lambda () expr)
;         (lambda args #f))))
;    ((define-values (var) expr)
;     (define var expr))
;    ((define-values (var0 var1 ... varn) expr)
;     (begin
;       (define var0
;         (call-with-values (lambda () expr) list))
;       (define var1
;         (let ((v (cadr var0)))
;           (set-cdr! var0 (cddr var0))
;           v))
;       ...
;       (define varn
;         (let ((v (cadr var0)))
;           (set! var0 (car var0))
;           v))))
;    ((define-values (var0 var1 ... . var-dot) expr)
;     (begin
;       (define var0
;         (call-with-values (lambda () expr) list))
;       (define var1
;         (let ((v (cadr var0)))
;           (set-cdr! var0 (cddr var0))
;           v))
;       ...
;       (define var-dot
;         (let ((v (cdr var0)))
;           (set! var0 (car var0))
;           v))))
;    ((define-values var expr)
;     (define var
;       (call-with-values (lambda () expr) list)))))

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

(define range
  (case-lambda
    ((e) (range 0 e))
    ((b e) (do ((r '() (cons e r))
                (e (- e 1) (- e 1)))
               ((< e b) r)))))
(write
  (range 3)) ; => (0 1 2)
(write
  (range 3 5)) ; => (3 4)

;(define-values (x y) (integer-sqrt 17))
;(write
;  (list x y)) ; => (4 1)
;
;(write
;  (let ()
;    (define-values (x y) (values 1 2))
;    (+ x y))) ;=> 3


; TODO: parameter objects
;(define (parameter-convert param value)
;  (let ((proc (parameter-converter param)))
;    (if (procedure? proc)
;        (proc value)
;        value)))
;
;(define (make-parameter init . o)
;  (let ((conv (and (pair? o) (car o))))
;    (%make-parameter (if conv (conv init) init) conv)))
;
;(define-syntax parameterize
;  (syntax-rules ()
;    ((parameterize ("step") old cons-new ((param value ptmp vtmp) ...) () body)
;     (let ((ptmp param) ...)
;       (let ((vtmp (parameter-convert ptmp value)) ...)
;         (let ((old (thread-parameters)))
;           (let ((new cons-new))
;             (dynamic-wind
;                 (lambda () (thread-parameters-set! new))
;                 (lambda () . body)
;                 (lambda () (thread-parameters-set! old))))))))
;    ((parameterize ("step") old cons-new args ((param value) . rest) body)
;     (parameterize ("step") old (cons (cons ptmp vtmp) cons-new) ((param value ptmp vtmp) . args) rest body))
;    ((parameterize ((param value) ...) . body)
;     (parameterize ("step") old (thread-parameters) () ((param value) ...) body))))
;
;(define radix
;  (make-parameter
;    10
;    (lambda (x)
;      (if (and (exact-integer? x) (<= 2 x 16))
;          x
;          (error "invalid radix")))))
;(define (f n) (number->string n (radix)))
;
;(write
;  (f 12)) ;=> "12"
;
;(write
;  (parameterize ((radix 2))
;    (f 12))) ;=> "1100"
;
;(write
;  (f 12)) ;=> "12"
