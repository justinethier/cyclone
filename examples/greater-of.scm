;; Test program for issue 248
(import (scheme base) (scheme write))

(define (greater-of x y) (if (> x y) x y))

(write (greater-of 0 1))
(newline)
(write (greater-of 1 0))
(newline)
