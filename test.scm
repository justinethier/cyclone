((lambda (x)
  ((lambda ()
    ((lambda (z)
      (+ x z)) 2)))) 1)
;;; Temporary testing, delete this once it works
; Need to rewrite the code to use this, and preserve the global def
;(define (test)
;  (call/cc
;    (lambda (return)
;      (return #t))))
;(write (test))
;
;; (write
;;   (with-exception-handler
;;     (lambda (con)
;;       (cond
;;         ((string? con)
;;          (display con))
;;         (else
;;          (display "a warning has been issued")))
;;       42)
;;     (lambda ()
;;       (+ (raise-continuable "should be a number") 23)
;;       )))
;; ;prints: should be a number
;; ;=> 65
;; 
;; (write
;;   (call/cc
;;     (lambda (k)
;;       (with-exception-handler
;;         (lambda (x)
;;           (display "condition: ")
;;           (write x)
;;           ;(newline)
;;           (k 'exception))
;;         (lambda ()
;;           (+ 1 (raise 'an-error)))))))
;; 
;; (with-exception-handler
;;     (lambda (x)
;;         (display "something went wrong\n"))
;;     (lambda ()
;;         (+ 1 (raise 'an-error))))
;; 
;; ;(define test '(a b))
;; ;(set-car! test '(1 2 3))
;; ;(write test)
;; ;(raise 'done)
;; ;(define (loop n)
;; ;  (cond
;; ;    ((= n 10000)
;; ;     (write test)
;; ;     (loop 0))
;; ;    (else
;; ;      (loop (+ n 1)))))
;; ;(loop 0)
