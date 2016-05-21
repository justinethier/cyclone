;; A temporary test file

(import (scheme base) (scheme write))

(let ((x 1))
  (define y 2)
  (write
    (+ x y)))

(define (test a)
  (write 3))

(test 4)
