;; Testing the next set of optimizations
;; To run: ./opt-test < opt-test.data
;;
;; Timings: T430
;; Baseline - 2.511
;;
(import (scheme base)
        (scheme write)
        (scheme read))
#;(let ((x (read))
      (y (read))
      (z (read))
      (iterations 10000000)
      (sum 0))
  (do ((i iterations (- i 1)))
      ((zero? i))
      (set! sum (+ sum sum (* x y z)))
      (set! sum (- sum sum (* x y z))))
  (write sum))

;; Take an expression containing a single function call and break it up
;; into many calls of 2 arguments each.
(define (->dyadic expr)
  (cond
    ((< (length expr) 4)
     expr)
    (else
     (let ((fnc (car expr)))
       (foldl
         (lambda (x acc)
           (list fnc acc x))
         `(,fnc ,(cadr expr) ,(caddr expr))
         (cdddr expr))))))
    
(write (->dyadic '(+ 1)))
(write (->dyadic '(+ 1 2)))
(write (->dyadic '(+ 1 2 3)))
(write (->dyadic '(+ 1 2 3 4)))
;(write
;  (foldl
;    (lambda (x acc)
;     (list 'Cyc-fast-plus acc x))
;    '(Cyc-fast-plus 1 2)
;    '(3 4 5)))
