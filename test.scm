;; Temporary testing, delete this once it works
(define (mywrite x)
  (write x))

(define (call fn a)
 (fn a))

(call write 'hello)

;; Demonstrate sending an interpreted function to compiled code
;; I think in order for this to work, the compiled code would have to
;; detect an interpreted proc, and use eval to execute it
;(eval '(call (lambda (x) x) (display (+ 1 1))))
;(eval '(call write 1))
(eval '(call mywrite 1))
