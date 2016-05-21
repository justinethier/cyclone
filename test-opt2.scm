(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme lazy)
        (scheme read)
        (scheme write)
        (scheme eval)
)

(define (assert:equal msg actual expected)
  (if (not (equal? actual expected))
      (error "Unit test failed [" msg "] actual [" actual "] expected [" expected "]")
      (set! *num-passed* (+ *num-passed* 1))))
;; Adder example
(define (make-adder x)
  (lambda (y) (+ x  y)))
(define decrement (make-adder -1))
(assert:equal "Adder #2" (decrement 42) 41)

(assert:equal "Application example"
  ((lambda (x) x) (+ 41 1))
  42)

