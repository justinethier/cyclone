; TODO:
; try having multiple producers writing to a queue, and 
; a consumer reading from it
; an important consideration is how to let the producers write values, since they would be on their stacks! 
; ideally do not want the application to have to worry about what value came from what thread...

(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (srfi 18))


(define *lock* (make-mutex))
(define *queue* (list))

(define (producer)
  (let loop ((n 10))
    (cond
      ((> n 0)
       (mutex-lock! *lock*)
       (set! *queue* (cons n *queue*))
       (mutex-unlock! *lock*)
       (loop (- n 1))))))

(define (consumer)
  (let loop ()
    (write (list (null? *queue*) *queue*))
    (define sleep? #f)
    (mutex-lock! *lock*)`
    (cond
      ((not (null? *queue*))
       (write (car *queue*))
       (set! *queue* (cdr *queue*)))
      (else
       (set! sleep? #t)))
    (mutex-unlock! *lock*)
    (if sleep? (thread-sleep! 1000))
    (loop)))

(thread-start! (make-thread producer))
(producer)   
(consumer)

