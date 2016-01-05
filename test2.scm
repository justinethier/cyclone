(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define lock (make-mutex))
(mutex-lock! lock)
(mutex-unlock! lock)

;; A program to prove if cooperation is working, or if it
;; is blocked by another thread. The (read) causes the main
;; thread to block. The collector should be notified prior 
;; to the blocking call being made, and the collector should
;; be able to cooperate on the main thread's behalf:
(define tmp '())
(thread-start! 
  (make-thread 
    (lambda () 
      (write 'start-mem-producer-thread)
      (letrec ((loop (lambda ()
                      (set! tmp (cons "cons" tmp))
                      ;(write tmp)
                      (cond
                       ((> (length tmp) 1000)
                        ;(write "resetting tmp")
                        (set! tmp '()))
                       (else #f))
                      (loop))))
      (loop))
    )))

(thread-start! 
  (make-thread 
    (lambda () 
      (write 'start-mutex-thread)
      (letrec ((loop (lambda ()
                      (let ((rv (mutex-lock! lock)))
                        (write (list 'mutex-result rv))
                        (mutex-unlock! lock))
                      ;(loop)
                      )))
      (loop))
    )))

; main thread loop
(letrec ((loop (lambda ()
                (mutex-lock! lock)
                (let ((rv (read)))
                  (write `(read ,rv)))
                (mutex-unlock! lock)
                (thread-sleep! 1000)
                (loop))))
(loop))


