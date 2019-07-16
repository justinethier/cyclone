;; TODO: support for concurrent delay and promise as implemented by Clojure
;; See: https://purelyfunctional.tv/guide/clojure-concurrency/

(import (scheme write) (scheme base) (cyclone concurrent) (srfi 18))

(define-record-type <shared-delay>
  (%make-shared-delay done result lock)
  shared-delay?
  (done sd:done sd:set-done!)
  (value sd:value sd:set-value!) ;; Either thunk or result
  (lock sd:lock sd:set-lock!))

(define (make-shared-delay thunk)
  (make-shared 
    (%make-shared-delay #f thunk (make-mutex))))

(define (shared-delay-deref d)
  (when (not (shared-delay? d))
    (error "Expected future but received" d))
  (mutex-lock! (sd:lock d))
  (cond
    ((sd:done d) 
     (sd:value d))
    (else
     (sd:set-value! d 
       (make-shared ((sd:value d)))) ;; Exec thunk and store result
     (sd:set-done! d #t)))
  (mutex-unlock! (sd:lock d))
)

(define (shared-delay-realized? obj)
  (let ((rv #f))
    (mutex-lock! (sd:lock obj))
    (set! rv (sd:done obj))
    (mutex-unlock! (sd:lock obj))
    rv))

(define-syntax shared-delay
  (er-macro-transformer
   (lambda (expr rename compare)
     `(make-shared-delay (lambda () ,(cadr expr))))))


(define (test)
  (write '(testing)) (newline)
  'done)

(define d (shared-delay (test)))
(write (shared-delay-deref d))(newline)
(write (shared-delay-deref d))(newline)
(write (shared-delay-deref d))(newline)


;; Promises

(define-record-type <shared-promise>
  (%make-shared-promise done value lock cv)
  shared-promise?
  (done sp:done sp:set-done!)
  (value sp:value sp:set-value!)
  (lock sp:lock sp:set-lock!)
  (cv sp:cv sp:set-cv!))

(define (make-shared-promise)
  (make-shared
    (%make-shared-promise #f #f (make-mutex) (make-condition-variable))))

;; Blocks until given promise has a value, and returns that value.
(define (shared-promise-deref obj)
  (when (not (shared-promise? obj))
    (error "Expected shared promise but received" obj))
  (mutex-lock! (sp:lock obj))
  (if (sp:done obj)
      (mutex-unlock! (sp:lock obj))
      (mutex-unlock! (sp:lock obj) (sp:cv obj))) ;; wait until value is ready
  (sp:value obj))

(define (shared-promise-realized? obj)
  (let ((rv #f))
    (mutex-lock! (sp:lock obj))
    (set! rv (sp:done obj))
    (mutex-unlock! (sp:lock obj))
    rv))

;; Delivers `value` to shared promise `obj` and unblocks waiting threads.
;; Has no effect if a value has already been delivered.
(define (deliver obj value)
  (when (not (shared-promise? obj))
    (error "Expected shared promise but received" obj))
  (mutex-lock! (sp:lock obj))
  (when (not (sp:done obj))
    (sp:set-value! obj (make-shared value)) 
    (sp:set-done! obj #t))
  (mutex-unlock! (sp:lock obj))
  (condition-variable-broadcast! (sp:cv obj)))


(define tp (make-thread-pool 4))
(define sp (make-shared-promise))
(thread-pool-push-task! tp
  (lambda ()
    (thread-sleep! 1)
    (deliver sp (list (+ 1 2 3)))))
(write
  `(received promised value of ,(shared-promise-deref sp)))
(newline)

