;;;; Example of using multiple threads to sum a variable without synchronization.
;;;; Returns inconsistent and wrong results due to a lack of thread coordination.
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define *sum* 0)

(define (sum-loop n)
  (set! *sum* (+ *sum* 1))
  (if (zero? n)
      'done
      (sum-loop (- n 1))))

(define (sum-entry-pt)
  (sum-loop (* 100 100 100)))

;; Thread - Do something, then let main thread know when we are done
(define t1 (make-thread sum-entry-pt))
(define t2 (make-thread sum-entry-pt))
(define t3 (make-thread sum-entry-pt))
(define t4 (make-thread sum-entry-pt))
(define t5 (make-thread sum-entry-pt))
(define t6 (make-thread sum-entry-pt))
(thread-start! t1)
(thread-start! t2)
(thread-start! t3)
(thread-start! t4)
(thread-start! t5)
(thread-start! t6)

(thread-join! t1)
(thread-join! t2)
(thread-join! t3)
(thread-join! t4)
(thread-join! t5)
(thread-join! t6)
(display "main thread done, sum = ")
(display *sum*)
(newline)
