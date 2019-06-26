;; Example of multiple threads using a shared queue 
(import
  (scheme base)
  (scheme write)
  (shared-queue)
  (srfi 18)
  (cyclone concurrent))

(define q (make-queue))
(define (consume)
  (let ((val (queue-remove! q)))
    (write `(removed ,val))
    (thread-sleep! 2)))
(define t1 (make-thread consume))
(define t2 (make-thread consume))

(thread-start! t1)
(thread-start! t2)

(thread-sleep! 1)
(queue-add! q 'a)
(queue-add! q 'b)

(thread-join! t1)
(thread-join! t2)
(write "done")
