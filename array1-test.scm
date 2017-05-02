;;; A temporary test file, can inlining be done more efficiently here?
;; if this inline can be done, try with full-up array1

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result) ;; TODO: check generated code, can this >= be inlined???
    (vector-set! result i i)))

(write (create-x 10))
