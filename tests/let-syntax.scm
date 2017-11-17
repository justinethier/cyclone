(import (scheme base))

;; (let-syntax ((given-that (syntax-rules ()
;; ((given-that test stmt1 stmt2 ...)
;; (if test
;; (begin stmt1
;; stmt2 ...))))))
;; (let ((if #t))
;; (given-that if (set! if 'now))
;; if)) ;; => now

(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m)))) ;; Should be outer
