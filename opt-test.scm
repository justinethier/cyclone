;; Testing optimizations
;;
;; Timings: T430
;; Baseline - 2.511
;;
(import (scheme base)
        (scheme write)
        (scheme read))
(let ((x (read))
      (y (read))
      (z (read))
      (iterations 10000000)
      (sum 0))
  (do ((i iterations (- i 1)))
      ((zero? i))
      (set! sum (+ sum sum (* x y z)))
      (set! sum (- sum sum (* x y z))))
  (write sum))
