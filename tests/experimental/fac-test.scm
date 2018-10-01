(import (scheme base)
        (scheme write)
)

;(define (test n) 
;  ((lambda (x) (+ x x))
;   n))
;(write (test 10))

(define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))

(write (fac 10))
