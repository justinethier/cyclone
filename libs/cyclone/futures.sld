;; Temporary test file for futures
;; See:
;; https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/future?
;; https://purelyfunctional.tv/guide/clojure-concurrency/#future

(define-library (futures)
  (import (scheme base)
          (scheme write)
          ;(cyclone concurrent)
          (srfi 18)
  )
  (export
    future?
    future
    future-call
    future-deref
  )
  (begin

  (define-record-type <future>
    (make-future done result lock)
    future?
    (done get-done set-done!)
    (result get-result set-result!)
    (lock get-lock set-lock!))

  ;; macro: (future expr ...)
  (define-syntax future
    (er-macro-transformer
      (lambda (expr rename compare)
        `(future-call (lambda () ,@(cdr expr))))))

;; From the clojure docs:
;;
;; Takes a function of no args and yields a future object that will
;; invoke the function in another thread, and will cache the result and
;; return it on all subsequent calls to deref/@. If the computation has
;; not yet finished, calls to deref/@ will block, unless the variant
;; of deref with timeout is used. See also - realized?.
(define (future-call thunk)
  (let* (
         (lock (make-mutex))
         (ftr (make-future #f #f lock))
         (tfnc (lambda ()
                 (mutex-lock! lock) 
                 (let ((result (thunk))) ;; TODO: Catch exceptions (?)
                   (set-result! ftr result)
                   (set-done! ftr #t)
                   (mutex-unlock! lock) 
                 )))
         (t (make-thread tfnc))
        )
    (thread-start! t)
    ftr))

;;(define (future-done? ftr)
;;  (when (not (future? ftr))
;;    (error "Expected future but received" ftr))
;; TODO: may be a good candidate for a timed mutex lock, just return #f if minimum timeout is exceeded
;;)

;; TODO: (future-cancel ftr)
;; TODO: (future-cancelled? ftr)

;;TODO: custom deref but eventually need to fold this functionality back into the main one
(define (future-deref ftr)
  (when (not (future? ftr))
    (error "Expected future but received" ftr))
  (let ((result #f))
    (mutex-lock! (get-lock ftr))
    (set! result (get-result ftr))
    (mutex-unlock! (get-lock ftr))
    result))

))
