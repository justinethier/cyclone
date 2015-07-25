(import (scheme base)
        (scheme write))

(define (fac n) 
  (if (= n 0) 
    1 
    (* n (fac (- n 1)))))

(write
  (fac 10))
