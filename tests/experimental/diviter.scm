;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))

(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))

(define (main)
  (let* ((count 1000000)
         (input1 1000)
         (output 500)
         (s2 (number->string count))
         (s1 (number->string input1))
         (ll (create-n (hide count input1)))
         (name "diviter"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (iterative-div2 ll))
     (lambda (result) (equal? (length result) output)))))
