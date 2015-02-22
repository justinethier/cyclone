;; This should run forever using a constant amount of memory
;; and max CPU:

;; Original program:
;; (define (foo) (bar))
;; (define (bar) (foo))
;; (foo)

(letrec ((foo (lambda () (bar)))
         (bar (lambda () (foo))))
    (foo))
