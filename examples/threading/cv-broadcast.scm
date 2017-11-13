;;;; An example of using a condition variable to wake other threads.
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define *done* #f)
(define cv (make-condition-variable))
(define m (make-mutex))

(define (trace msg)
  (display msg)
  (newline))

;; Thread - Sleep, then wake other threads up via broadcast
(thread-start!
  (make-thread
    (lambda ()
      (thread-sleep! 3)
      (set! *done* #t)
      (condition-variable-broadcast! cv)
      (trace "broadcast thread done"))))

;; Thread - wait for broadcast
(thread-start!
  (make-thread
    (lambda ()
      (let loop ()
        (mutex-lock! m)
        (cond
          (*done*
            (trace "waiting thread done")
            (mutex-unlock! m))
          (else
            (trace "waiting thread - waiting for cv")
            (mutex-unlock! m cv)
            (loop)))))))

;; Main thread - wait for broadcast
(let loop ()
  (mutex-lock! m)
  (cond
    (*done*
      (mutex-unlock! m)
      (thread-sleep! 0.5)
      (trace "main thread done"))
    (else
      (mutex-unlock! m cv) ;; Wait on cv
      (loop))))
