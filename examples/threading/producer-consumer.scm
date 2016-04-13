;; An example program having multiple producers concurrently writing to a queue 
;; and a single consumer reading from the queue.
(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (srfi 18))

;; Global queue and a lock to coordinate access to it.
;; Note ->heap is used to ensure any objects accessed by multiple threads are 
;; not on a thread's local stack, since those objects can be moved at any time
;; by the thread's minor GC.
(define *queue* (->heap (list)))
(define *lock* (make-mutex))

(define (producer)
  (let loop ((n 100))
    (cond
      ((> n 0)
       (mutex-lock! *lock*)
       (display (cons 'a *queue*))
       (newline)
       (set! *queue* (->heap (cons (->heap n) *queue*)))
       (mutex-unlock! *lock*)
       (loop (- n 1)))
      (else
        (display "producer thread done")
        (newline)))))

(define (consumer)
  (let loop ()
    ;(display (list (null? *queue*) *queue*))
    ;(newline)
    (define sleep? #f)
    (mutex-lock! *lock*)
    (cond
      ((not (null? *queue*))
       (display (car *queue*))
       (newline)
       (set! *queue* (cdr *queue*)))
      (else
       (display "consumer sleeping")
       (newline)
       (set! sleep? #t)))
    (mutex-unlock! *lock*)
    (if sleep? (thread-sleep! 1000))
    (loop)))

(thread-start! (make-thread producer))
(thread-start! (make-thread producer))
(thread-start! (make-thread producer))
;(producer)   
(consumer)
;(read)
