(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (srfi 18))

;; should not be necessary, just testing
(define m (make-mutex))

(define (write-forever val)
  (mutex-lock! m)
  (write val)
  (mutex-unlock! m)
  (write-forever val))

(define (make-writer val)
  (lambda () (write-forever val)))

(thread-start!
  (make-thread
    (make-writer "thread 1")))
((make-writer 'main))
;(read)
