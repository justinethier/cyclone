(define (fac n) 
  (if (= n 0) 
    1 
    (* n (fac (- n 1)))))

(fac 10)
