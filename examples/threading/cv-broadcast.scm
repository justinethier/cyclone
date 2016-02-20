;;;; An example of using condition variable broadcast to wake other threads.
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define cv (make-condition-variable))
(define m (make-mutex))

;; Thread - Sleep, then wake other threads up via broadcast
(thread-start!
  (make-thread
    (lambda ()
      (write "started thread")
      (thread-sleep! 3000)
      (condition-variable-broadcast! cv)
      (write "thread done"))))

;; Thread - wait for broadcast
(thread-start!
  (make-thread
    (lambda ()
      (write "started waiting thread")
      (mutex-lock! m)
      (write "register waiting thread cv")
      (mutex-unlock! m cv)
      (write "waiting thread done"))))

;; Main thread - wait for broadcast
(mutex-lock! m)
(mutex-unlock! m cv) ;; Wait on cv
(write "main thread done")
(thread-sleep! 500)
