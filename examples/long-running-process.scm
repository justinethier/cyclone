(import (scheme base)
        (scheme eval)
        (scheme write)
        (scheme cyclone transforms)
        (srfi 18))

(let loop ((i 0))
  (expand
   '((define (consumer)
      (let loop ((x 1))
        ;(write (list (null? *queue*) *queue*))
        (define sleep? #f)
        (mutex-lock! *lock*)
        (cond
          ((not (null? *queue*))
           (write (car *queue*))
           (set! *queue* (cdr *queue*)))
          (else
           (write "consumer sleeping")
           (set! sleep? #t)))
        (mutex-unlock! *lock*)
        (if sleep? (thread-sleep! 1000))
        (loop)))) 
   '())

  (write `(,i))
  (thread-sleep! 5)
  (loop (+ i 1)))
