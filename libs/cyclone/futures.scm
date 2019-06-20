;; Temporary test file for futures
;; See:
;; https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/future?
;; https://purelyfunctional.tv/guide/clojure-concurrency/#future

(import (scheme base)
        (scheme write)
        (cyclone concurrency)
        (srfi 18)
)

(define *future-sym* (string->symbol "  future  "))
(define (future? obj)
  (and (vector? obj) (eq? (vector-ref obj 0) *future-sym*)))


;; TODO: macro (future expr ...)

;; From clojure docs:
; Takes a function of no args and yields a future object that will
; invoke the function in another thread, and will cache the result and
; return it on all subsequent calls to deref/@. If the computation has
; not yet finished, calls to deref/@ will block, unless the variant
; of deref with timeout is used. See also - realized?.

(define (future-call thunk)
  (let ((ftr (vector *future-sym* 'todo)))
    ;; TODO: setup and call the thread here
    ftr))


TODO: custom deref but eventually need to fold that functionality back into the main one
