;;;; A simple example of using a condition variable to simulate thread-join
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define cv (make-condition-variable))
(define m (make-mutex))

;; Thread - Do something, then let main thread know when we are done
(thread-start!
  (make-thread
    (lambda ()
      (display "started thread")
      (newline)
      (thread-sleep! 3000)
      (display "thread done")
      (newline)
      (condition-variable-broadcast! cv))))

;; Main thread - wait for thread to broadcast it is done
(mutex-lock! m)
(mutex-unlock! m cv) ;; Wait on cv
(display "main thread done")
(newline)
(thread-sleep! 500)
