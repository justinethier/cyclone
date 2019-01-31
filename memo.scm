;; Temporary test file, try with the ack function
(import (scheme base) 
        (srfi 69)
        (scheme write))

 (define (memoize function) 
   (let ((table (make-hash-table))) ;(make-equal?-map))) 
     (lambda args 
       (apply values 
              ;(map-get table 
              (hash-table-ref table 
                       args 
                       ;; If the entry isn't there, call the function.    
                       (lambda () 
                         (call-with-values 
                           (lambda () (apply function args)) 
                           (lambda results 
                             ;(map-put! table args results) 
                             (hash-table-set! table args results) 
                             results)))))))) 

(define (fnc x y) (+ x y))
(define mfnc (memoize fnc))

(write (mfnc 1 1)) (newline)
(write (mfnc 1 1)) (newline)

; Original versions:
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; Fast versions:
;(define ack (memoize _ack))
;(define (_ack m n)
;  (cond ((= m 0) (+ n 1))
;        ((= n 0) (ack (- m 1) 1))
;        (else (ack (- m 1) (ack m (- n 1))))))
;
;(define fib (memoize _fib))
;(define (_fib n)
;  (if (< n 2)
;      n
;      (+ (fib (- n 1))
;         (fib (- n 2)))))

(write (ack 3 12))
(newline)
(write (fib 40))
