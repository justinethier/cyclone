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

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))
(write
  (ack 3 12))

(define mack (memoize _mack))
(define (_mack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (mack (- m 1) 1))
        (else (mack (- m 1) (mack m (- n 1))))))
(write
  (_mack 3 12))
