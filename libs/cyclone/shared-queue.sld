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
  queue-wait-count
  queue-capacity
  queue-empty?

  make-thread-pool 
  thread-pool?
  thread-pool-size
  thread-pool-idling-count
  thread-pool-idling?
;  thread-pool-push-task!
;  ;;thread-pool-wait-all!
;  ;;thread-pool-release!
)

(begin

(define *default-table-size* 64)

;TODO: how will data structure work?
;probably want a circular queue, add at end and remove from start
;need to keep track of those positions, they may wrap around the end
;of the queue
;so we need - start, end
;capacity is length of the vector
;if start == end, vector is empty
;if start == end after an add, then vector is full, need to resize

  (define-record-type <queue>
    (%make-queue store start end wait-count lock cv)
    queue?
    (store q:store q:set-store!)
    (start q:start q:set-start!)
    (end q:end q:set-end!)
    (wait-count q:wait-count q:set-wait-count)
    (lock q:lock q:set-lock!)
    (cv q:cv q:set-cv!)
    )

(define (make-queue)
  (make-shared
    (%make-queue
      (make-vector *default-table-size* #f)
      0
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
  (%queue-add! q (make-shared obj))
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

;; Blocks if queue is empty (!)
;; should we have a failsafe if the same thread that is doing adds, then
;; does a blocking remove??
(define (queue-remove! q)
  (let loop ((waiting #f))
    (mutex-lock! (q:lock q))
    ;; If thread was previously waiting, clear that status
    (when waiting
      (set! waiting #f)
      (q:set-wait-count q (- (q:wait-count q) 1)))

    (cond
      ((= (q:start q) (q:end q))
       ;; Let Q know we are waiting
       (set! waiting #t)
       (q:set-wait-count q (+ (q:wait-count q) 1))
       ;; Wait for CV, indicating data is ready
       (mutex-unlock! (q:lock q) (q:cv q))
       (loop waiting))
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

(define (queue-wait-count q)
  (define result 0)
  (mutex-lock! (q:lock q))
  (set! result (q:wait-count q))
  (mutex-unlock! (q:lock q))
  result)

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





  (define-record-type <thread-pool>
    (%make-thread-pool jobq threads num-threads )
    thread-pool?
    (jobq tp:jobq tp-set-jobq!)
    (threads tp:threads tp:set-threads!)
    (num-threads tp:num-threads tp:set-num-threads!)
    )

(define (default-handler err) #f)
(define (%make-thread-pool-thread q) ;; TODO: optional exception handler
  (make-thread 
    (lambda ()
        (let loop ()
          (with-handler
            default-handler ;; TODO: allow passing this in
            (let ((thunk (queue-remove! q)))
              (thunk))
        )))))

(define (make-thread-pool size)
  (let ((tp (%make-thread-pool (make-queue) '() size)))
    (do ((i size (- i 1))) 
        ((zero? i)) 
      (let ((t (%make-thread-pool-thread (tp:jobq tp))))
        (tp:set-threads! (cons t (tp:threads)))
        (thread-start! t)))
    (share-all!)
    tp))

(define (thread-pool-size tp)
  (queue-size (tp:jobq tp)))

(define (thread-pool-idling-count tp)
  (queue-wait-count (tp:jobq tp)))

(define (thread-pool-idling? tp)
  (> (thread-pool-idling-count tp) 0))

; TODO:  thread-pool-push-task!

; ?? - thread-pool-wait-all!

))
