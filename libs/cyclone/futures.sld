;; Temporary test file for futures
;; See:
;; https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/future?
;; https://purelyfunctional.tv/guide/clojure-concurrency/#future

(define-library (futures)
  (import (scheme base)
          (scheme write)
          (cyclone concurrent)
          (srfi 18)
  )
  (export
    future?
    future-call
    future-deref
  )
  (begin

;(define *future-sym* (string->symbol "  future  "))
;(define (future? obj)
;  (and (vector? obj) (eq? (vector-ref obj 0) *future-sym*)))
  (define-record-type <future>
    (make-future done result lock)
    future?
    (done get-done set-done!)
    (result get-result set-result!)
    (lock get-lock set-lock!))


;; TODO: macro (future expr ...)

;; From clojure docs:
; Takes a function of no args and yields a future object that will
; invoke the function in another thread, and will cache the result and
; return it on all subsequent calls to deref/@. If the computation has
; not yet finished, calls to deref/@ will block, unless the variant
; of deref with timeout is used. See also - realized?.

(define (future-call thunk)
  (let* (
         (lock (make-mutex))
         (ftr (make-future #f #f lock)
             ; (vector 
             ;   *future-sym*  ;; Type indicator
             ;   #f            ;; Done?
             ;   #f            ;; Result
             ;   lock)
         )
         (tfnc (lambda ()
                 (mutex-lock! lock) 
                 (let ((result (thunk))) ;; TODO: Catch exceptions (?)
                   (set-done! ftr #t)
                   (set-result! ftr result)
                   ;(vector-set! ftr 1 #t) ;; Done
                   ;(vector-set! ftr 2 result)
                   (mutex-unlock! lock) 
                 )))
         (t (make-thread tfnc))
        )
    (thread-start! t)
    ftr))


;;TODO: custom deref but eventually need to fold this functionality back into the main one
(define (future-deref ftr)
  (when (not (future? ftr))
    (error "Expected future but received" ftr))
  (let ((result #f))
    (mutex-lock!   (get-lock ftr)) ;(vector-ref ftr 3))
    (set! result   (get-result ftr)) ;(vector-ref ftr 2))
    (mutex-unlock! (get-lock ftr)) ;(vector-ref ftr 3))
    result))

))
