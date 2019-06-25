;;;; A temporary test file, this code needs to be separated into a library and a set of unit tests

;TODO: a queue that can be shared among threads
;objects on the queue must be shared objects, possibly immutable (but more flexible if not)
;consider making the queue vector-based, and increase size by 2 if capacity is exceeded?
;
;supported operations:
;
;- queue?
;- (queue ...) constructor
;- queue-add! - add item to the queue
;- queue-remove! - remove item (when to block? would be nice if we can block until an item becomes available)
;            maybe block by default, but have an optional timeout
;- queue-get - ?? 
;- queue-clear!
;- queue->list
;- queue-size (current length)
;- queue-capacity (max size until resize occurs)
;- queue-empty?

(import (scheme base)
        (cyclone concurrent)
        (srfi 18)
        (scheme write))

(define *default-table-size* 64)

TODO: how will data structure work?
probably want a circular queue, add at end and remove from start
need to keep track of those positions, they may wrap around the end
of the queue
so we need - start, end
capacity is length of the vector
if start == end, vector is empty
if start == end after an add, then vector is full, need to resize

TODO: should create a corresponding file of tests for this

  (define-record-type <queue>
    (%make-queue store size lock)
    queue?
    (store q:store q:set-store!)
    (size q:size q:set-size!)
    (lock q:lock q:set-lock!))

(define (make-queue)
  (make-shared
    (%make-queue
      (make-vector *default-table-size* #f)
      0
      (make-mutex))))

(define (queue . elems)
  (let ((q (make-queue)))
    (for-each
      (lambda (elem)
        (%queue-add! q elem))
      (reverse elems))))

;; Inner add, assumes we already have the lock
(define (%queue-add! q obj)
  (vector-set! (q:store q) (q:size q) (make-shared obj))
  (cond
    ((= (q:size q) (vector-length (q:store)))
     (%queue-resize! q))
    (else
      (q:set-size! q (+ (q:size q) 1))))
)

(define (queue-add! q obj)
  (mutex-lock! (q:lock q))
  (%queue-add! q obj)
  (mutex-unlock! (q:lock q))
)

(define (%queue-resize! q)
  ;; TODO: assumes we already have the lock
  ;; TODO: error if size is larger than fixnum??
  (let ((old-store (q:store q))
        (new-store (make-vector (* (vector-length old-store) 2) #f)))
    (q:set-size! q 0)
    (let loop ((i (vector-length old-store)))
      (when (not (zero? i))
        (%queue-add! q (vector-ref 
        (loop (- i 1)))))
)

;- (queue ...) constructor
;- queue-add! - add item to the queue
;- queue-remove! - remove item (when to block? would be nice if we can block until an item becomes available)
;            maybe block by default, but have an optional timeout
;- queue-get - ?? 
;- queue-clear!
;- queue->list
;- queue-size (current length)
;- queue-capacity (max size until resize occurs)
;- queue-empty?
