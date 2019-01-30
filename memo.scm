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
