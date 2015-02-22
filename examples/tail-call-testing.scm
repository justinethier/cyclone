;; A program to use all available memory, and eventually crash
(letrec ((foo (lambda (x) 
                (write (length x)) 
                (bar (cons 1 x))))
         (bar (lambda (x) (foo (cons 1 x)))))
    (foo '()))

;; TODO: try rewriting it so memory is reclaimed. Does it run
;; forever now?
;(letrec ((foo (lambda (x) 
;                (write (length x)) 
;                (bar (cons 1 x))))
;         (bar (lambda (x) (foo (cons 1 x)))))
;    (foo '()))
