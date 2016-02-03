;; A temporary test file
(import (scheme base)
        (scheme write)
        (srfi 9)
        )

;; TODO: seems begins are not spliced when part of an applied lambda??
((lambda ()

(define-record-type <pare>
  (kons x y)
  pare?
  (x kar) ;TODO: set-kar!)
  (y kdr))

(write
  (list
    (pare? (kons 1 2)) ; =. #t
    (pare? (cons 1 2)) ; =. #f
;    (kar (kons 1 2))   ; =. 1
;    (kdr (kons 1 2))   ; =. 2
;    (let ((k (kons 1 2)))
;      (set-kar! k 3)
;      (kar k)) ;=. 3
))

;(define <pare> (register-simple-type <pare> #f (quote (x y)))) 
;(define pare? (make-type-predicate pare? <pare>)) 
;(define kons 
;  ((lambda (%make) 
;     (lambda (x y) 
;       ((lambda (res) 
;          (slot-set! <pare> res (type-slot-offset <pare> (quote y)) y) 
;          (slot-set! <pare> res (type-slot-offset <pare> (quote x)) x) 
;          res) 
;        (%make)))) 
;   (make-constructor "kons" <pare>))) 
;(write 
;  (list 
;    (kons 1 2)
;    (pare? (kons 1 2)) 
;    (pare? (cons 1 4))
;))
))

;(((lambda () 
;(define <pare> (register-simple-type <pare> #f (quote (x y)))) 
;(define pare? (make-type-predicate pare? <pare>)) 
;(define kons ((lambda (%make) (lambda (x y) ((lambda (res) (slot-set! <pare> res (type-slot-offset <pare> (quote y)) y) (slot-set! <pare> res (type-slot-offset <pare> (quote x)) x) res) (%make)))) (make-constructor "kons" <pare>))) (write (list (pare? (kons 1 2)) (pare? (cons 1 2)))))))
;(define (make-lambda)
;  (lambda (a b c) (write (+ a b c))))
;(define test (make-lambda))
;(test 1 2 3)
