;;; Temporary testing, delete this once it works
;(define (call2 fn x y)
;  (write
;    (fn x y)))
;
;(define (call fn a)
; (fn a))
;
;(call write 'hello)
;
;;; Demonstrate sending an interpreted function to compiled code
;;; I think in order for this to work, the compiled code would have to
;;; detect an interpreted proc, and use eval to execute it
;
;;; TODO: to debug this, may try placing printfs in runtime's apply
;(eval '(call2 (lambda (x y) (+ y x)) 2 3))
;;(eval '(call (lambda (x) (+ 1 x)) (display (+ 1 1))))
;;(eval '(call write 1))
;;(eval '(call mywrite 1))
;
;(eval '(define (a x) x))
;(eval '(a 1))
;(eval '(a 1))
;(eval '(begin (define (a z) z) (a 1) (a 1)))

; TODO: demonstrates problem of using call/cc within a global.
; all globals are created with a k parameter, EG:
; ((lambda (call/cc)
;    (define test
;      (lambda (k$104)
;
; Need to rewrite the code to use this, and preserve the global def
(define (test)
  (call/cc
    (lambda (k)
      (k #t))))
(write (test))

;(write
;  (with-exception-handler
;    (lambda (con)
;      (cond
;        ((string? con)
;         (display con))
;        (else
;         (display "a warning has been issued")))
;      42)
;    (lambda ()
;      (+ (raise-continuable "should be a number") 23)
;      )))
;;prints: should be a number
;;=> 65
;
;(write
;  (call/cc
;    (lambda (k)
;      (with-exception-handler
;        (lambda (x)
;          (display "condition: ")
;          (write x)
;          ;(newline)
;          (k 'exception))
;        (lambda ()
;          (+ 1 (raise 'an-error)))))))
;
;(with-exception-handler
;    (lambda (x)
;        (display "something went wrong\n"))
;    (lambda ()
;        (+ 1 (raise 'an-error))))
;
;(define test '(a b))
;(set-car! test '(1 2 3))
;(write test)
;(raise 'done)
;(define (loop n)
;  (cond
;    ((= n 10000)
;     (write test)
;     (loop 0))
;    (else
;      (loop (+ n 1)))))
;(loop 0)
