;; Temporary testing, delete this once it works
(define (call2 fn x y)
  (write
    (fn x y)))

(define (call fn a)
 (fn a))

(call write 'hello)

;; Demonstrate sending an interpreted function to compiled code
;; I think in order for this to work, the compiled code would have to
;; detect an interpreted proc, and use eval to execute it

;; TODO: to debug this, may try placing printfs in runtime's apply
(eval '(call2 (lambda (x y) (+ y x)) 2 3))
;(eval '(call (lambda (x) (+ 1 x)) (display (+ 1 1))))
;(eval '(call write 1))
;(eval '(call mywrite 1))
