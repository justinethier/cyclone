;; A temporary test file

;; Notes:
;;Need to be careful about which lambda's we optimize, and ultimately remove
;;paramters from. As a start, we could try to just optimize cases where
;;there is an application of an anonymous lambda. those should be safe
;;to change?

(import (scheme base) (scheme write))

(let ((x 1))
  (define y 2)
  (write
    (+ x y)))

(define (test a)
  (write 3))

(test 4)
