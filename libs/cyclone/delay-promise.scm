;; TODO: support for concurrent delay and promise as implemented by Clojure
;; See: https://purelyfunctional.tv/guide/clojure-concurrency/

(import (scheme write) (scheme base) (cyclone concurrent) (srfi 18))

(define-record-type <delay>
  (%make-delay done result lock)
  delay?
  (done delay:done delay:set-done!)
  (value delay:value delay:set-value!) ;; Either thunk or result
  (lock delay:lock delay:set-lock!))

(define (make-delay thunk)
  (%make-delay #f thunk (make-mutex)))

(define (delay-deref d)
  (when (not (delay? d))
    (error "Expected future but received" d))
  (mutex-lock! (delay:lock d))
  (cond
    ((delay:done d) 
     (delay:value d))
    (else
     (delay:set-value! d 
       (make-shared ((delay:value d)))) ;; Exec thunk and store result
     (delay:set-done! d #t)))
  (mutex-unlock! (delay:lock d))
)

(define-syntax shared-delay
  (er-macro-transformer
   (lambda (expr rename compare)
     `(make-delay (lambda () ,(cadr expr))))))


(define (test)
  (write '(testing)) (newline)
  'done)

(define d (shared-delay (test)))
(write (delay-deref d))(newline)
(write (delay-deref d))(newline)
(write (delay-deref d))(newline)
