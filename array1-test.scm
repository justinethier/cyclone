;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result) ;; TODO: check generated code, can this >= be inlined???
    (vector-set! result i i)))

(write (create-x 10))
