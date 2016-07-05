;; An example program to test mutexes during GC cooperation
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18))

(define lock (make-mutex))
(mutex-lock! lock)
(mutex-unlock! lock)

;; Spin up a thread to constantly allocate memory and trigger major GC's
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

;; This thread is intended to block on mutex-lock, to test cooperation
;; on behalf of this thread
(thread-start! 
  (make-thread 
    (lambda () 
      (write 'start-mutex-thread)
      (letrec ((loop (lambda ()
                      (let ((rv (mutex-lock! lock)))
                        (write (list 'mutex-result rv))
                        (mutex-unlock! lock))
                        (thread-sleep! 1000)
                      (loop))))
      (loop))
    )))

;; Main thread loop, keep locking mutex and waiting for user input
(letrec ((loop (lambda ()
                (mutex-lock! lock)
                (let ((rv (read)))
                  (write `(read ,rv)))
                (mutex-unlock! lock)
                (thread-sleep! 1000)
                (loop))))
(loop))


