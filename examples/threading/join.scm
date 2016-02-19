;;;; A simple example of using a condition variable to simulate thread-join
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define cv (make-condition-variable))
(define m (make-mutex))

(thread-start!
  (make-thread
    (lambda ()
      (write "started thread")
      (thread-sleep! 5000)
      (condition-variable-broadcast! cv)
      (write "thread done"))))

(thread-start!
  (make-thread
    (lambda ()
      (write "started waiting thread")
      (mutex-lock! m)
      (write "register waiting thread cv")
      (mutex-unlock! m cv)
;; TODO: think this is never printed because mutex is locked after waking up..
      (write "waiting thread done"))))

(mutex-lock! m)
(mutex-unlock! m cv) ;; Wait on cv
(write "main thread done")
(thread-sleep! 500)
