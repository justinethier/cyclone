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
;- queue->list

(define-library (shared-queue)

(import (scheme base)
        (cyclone test)
        (cyclone concurrent)
        (srfi 18)
        (scheme write))

(export
  queue?
  make-queue
  queue
  queue-add!
  %queue-add! ;; DEBUG
  queue-remove!
  queue-clear!
  queue-size
  queue-capacity
  queue-empty?
)

(begin

(define *default-table-size* 4) ;; TODO: 64)

;TODO: how will data structure work?
;probably want a circular queue, add at end and remove from start
;need to keep track of those positions, they may wrap around the end
;of the queue
;so we need - start, end
;capacity is length of the vector
;if start == end, vector is empty
;if start == end after an add, then vector is full, need to resize

  (define-record-type <queue>
    (%make-queue store start end lock cv)
    queue?
    (store q:store q:set-store!)
    (start q:start q:set-start!)
    (end q:end q:set-end!)
    (lock q:lock q:set-lock!)
    (cv q:cv q:set-cv!)
    )

(define (make-queue)
  (make-shared
    (%make-queue
      (make-vector *default-table-size* #f)
      0
      0
      (make-mutex)
      (make-condition-variable)
      )))

(define (queue . elems)
  (let ((q (make-queue)))
    (for-each
      (lambda (elem)
        (%queue-add! q elem))
      (reverse elems))))

;; Increment an index, possibly back around to the beginning of the queue
(define (inc index capacity)
  (if (= index (- capacity 1))
      0
      (+ index 1)))

;; Inner add, assumes we already have the lock
(define (%queue-add! q obj)
  (vector-set! (q:store q) (q:end q) (make-shared obj))
  (q:set-end! q (inc (q:end q) (vector-length (q:store q))))
  (when (= (q:start q) (q:end q))
     (%queue-resize! q))
)

(define (queue-add! q obj)
  (mutex-lock! (q:lock q))
  (%queue-add! q obj)
  (mutex-unlock! (q:lock q))
  (condition-variable-signal! (q:cv q))
)

(define (%queue-resize! q)
 ;; (write "TODO: resize the queue")(newline)
  ;; TODO: assumes we already have the lock
  ;; TODO: error if size is larger than fixnum??
  (let* ((old-start (q:start q))
         (old-end (q:end q))
         (old-store (q:store q))
         (new-store (make-vector (* (vector-length old-store) 2) #f)))
    (q:set-start! q 0)
    (q:set-end! q 0)
    (q:set-store! q new-store)
    (let loop ((i 0)
               (start old-start))
      (when (not (= i (vector-length old-store)))
        (%queue-add! q (vector-ref old-store start))
        (loop (+ i 1) (inc start (vector-length old-store))))))
)

(define (queue-remove! q)
  (let loop ()
    (mutex-lock! (q:lock q))

    (cond
      ((= (q:start q) (q:end q))
       ;; Wait for CV, indicating data is ready
       (mutex-unlock! (q:lock q) (q:cv q))
       (loop))
      (else
        (let ((result (vector-ref (q:store q) (q:start q))))
          (q:set-start! q (inc (q:start q) (vector-length (q:store q))))
          (mutex-unlock! (q:lock q))
          result)))))

(define (queue-clear! q)
  (mutex-lock! (q:lock q))
  (q:set-start! q 0)
  (q:set-end! q 0)
  (mutex-unlock! (q:lock q)))

;; Return current length of the queue
(define (queue-size q)
  (define result 0)
  (mutex-lock! (q:lock q))
  (set! result (%queue-size q))
  (mutex-unlock! (q:lock q))
  result)

(define (%queue-size q)
  (let ((start (q:start q))
        (end (q:end q))
        (capacity (vector-length (q:store q))))
    (cond
      ((< end start) (+ (- capacity start) end))
      ((> end start) (- end start))
      (else 0)))) ;; (= end start)

(define (queue-empty? q)
  (= 0 (queue-size q)))

;; Return max size of the queue (until resize occurs)
(define (queue-capacity q)
  (define result 0)
  (mutex-lock! (q:lock q))
  (set! result (vector-length (q:store q)))
  (mutex-unlock! (q:lock q))
  result)

;- queue->list
))
