;;;; A simple example of using a condition variable to simulate thread-join
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

;(define cv (make-condition-variable))
;(define m (make-mutex))

;; Thread - Do something, then let main thread know when we are done
(define t 
  (make-thread
    (lambda ()
      (display "started thread")
      (newline)
      (thread-sleep! 3)
      (display "thread done")
      (newline)
      ;(thread-terminate!)
      )))

(define t2 
  (make-thread
    (lambda ()
      (display "started thread2")
      (newline)
      (thread-sleep! 4)
      (display "thread2 done")
      (newline)
      ;(thread-terminate!)
      )))
(thread-start! t)
(thread-start! t2)

(write (thread? t))
(thread-sleep! 1)
;; Main thread - wait for thread to broadcast it is done
(thread-join! t)
(thread-join! t2)
(display "main thread done")
(newline)
