;; A temporary test file
;; TODO: create a test suite
(import (scheme base) (scheme write))

(define x (exact (expt 2 29)))
(write (bignum? x))
(write (bignum? (+ x x)))
(write (+ x x))
(newline)
(set! x (+ x x))
(write
  (list x (+ x ) (+ 1 x)))
(newline)
(write
  (list 
    x 
    (+ 1.0 x ) 
    (+ x x) 
    (+ x 1.0) 
    (+ x 1)
    ))
(newline)
