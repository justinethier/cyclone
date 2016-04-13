;; Simple example demonstrating many threads concurrently writing data to stdout
(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (srfi 18))

(define (write-forever val)
  (display val)
  (write-forever val))

(define (make-writer val)
  (lambda () (write-forever val)))

(thread-start!
  (make-thread
    (make-writer "thread 1")))
(thread-start!
  (make-thread
    (make-writer 'thread-2)))
(thread-start!
  (make-thread
    (make-writer 'thread-3)))
((make-writer 'main))
;(read)
