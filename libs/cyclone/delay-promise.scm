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
  (%make-shared-delay #f thunk (make-mutex)))

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
  (%make-shared-promise #f #f (make-mutex) (make-condition-variable)))

(define (shared-promise-deref obj)
  ;; TODO: block on CV until ready
  ;; return sp:value
)

(define (deliver obj)
  ;; TODO: if not delivered, compute value and signal all on the cv
  ;;       else, do nothing
)

