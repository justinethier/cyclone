;; A temporary test file

;; Notes:
;;Need to be careful about which lambda's we optimize, and ultimately remove
;;paramters from. As a start, we could try to just optimize cases where
;;there is an application of an anonymous lambda. those should be safe
;;to change?

(import (scheme base) (scheme write))

(let ((x 1) (z 3))
  (define y 2)
  (define w 4)
  (write
    (+ w x y z)))

(define (test a)
  (write 3))

(test 4)
