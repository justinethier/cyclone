(import (scheme base)
        (scheme read))

(write `(read ,(+ 1 2 3)))
(write `(read ,(list 1 2 3)))
(write `(read ,@(list 1 2 3)))
;;`(read ,
(write (make-vector 4 #t))
(write (string->list "abc"))
(write (apply append '((1) (2) (3))))
(write #(a))
(write #(1 2 3))
(write #((1) (2) (3)))
(write (eq? #(1) #(1)))
(write (equal? #(1 1 1) (make-vector 3 1)))
(write '#(1))
(write '#())

(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 1 a 0 2)
(write (equal? b #(10 1 2 40 50)))
(call-with-values 
  (lambda () (values 1 1))
  (lambda (a) (write a)))

(write (Cyc-stdout))
