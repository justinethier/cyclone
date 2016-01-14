(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (srfi 18))

;; should not be necessary, just testing
;; TODO: noticed gc_move error with mutex... may be a GC issue
;(define m (make-mutex))

(define (write-forever val)
;  (mutex-lock! m)
  (write val)
; (list 1)
;  (mutex-unlock! m)
  (write-forever val))

(define (make-writer val)
  (lambda () (write-forever val)))

;; Try moving closures to heap prior to using threads
;; TODO: needed?
(Cyc-minor-gc)

(thread-start!
  (make-thread
    (make-writer "thread 1")))
(thread-start!
  (make-thread
    (make-writer 'thread-2)))
(thread-start!
  (make-thread
    (make-writer 'thread-3)))
;; TODO: when main runs the process crashes, but otherwise it seems stable. WTF?
((make-writer 'main))
(read)
