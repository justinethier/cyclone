;; This should run forever using a constant amount of memory
;; and max CPU:

(define (foo) (bar))
(define (bar) (foo))
(foo)

;; Another way to write it:
;; (letrec ((foo (lambda () (bar)))
;;          (bar (lambda () (foo))))
;;     (foo))
