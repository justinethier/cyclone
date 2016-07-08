; 1
; (())
; 2
; ((()) (()))
; 3
; (((()) (())) ((()) (())) ((()) (())))
;4
;((((()) (())) ((()) (())) ((()) (()))) (((()) (())) ((()) (())) ((()) (()))) (((()) (())) ((()) (())) ((()) (()))) (((()) (())) ((()) (())) ((()) (()))))

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

; Returns a list with n elements, all equal to x.

(define (make-test-list1 n x)
  (if (zero? n)
      '()
      (cons x (make-test-list1 (- n 1) x))))

; Returns a list of n lists, each consisting of n x's.
; The n elements of the outer list are actually the same list.

(define (make-test-tree1 n)
  (if (zero? n)
      '()
      (make-test-list1 n (make-test-tree1 (- n 1)))))



; Calls the thunk n times.

(define (iterate n thunk)
  (cond ((= n 1)
         (thunk))
        ((> n 1)
         (thunk)
         (iterate (- n 1) thunk))
        (else #f)))      


; DAG with much sharing.
; 10 is a good parameter for n.

(define (equality-benchmark1 n)
  (let ((x (make-test-tree1 n))
        (y (make-test-tree1 n)))

;(write 10) (newline)
;(write (make-test-tree1 10)) (newline)
;    (write x) 
;    (newline)
    (equal? x y)))
    ;(iterate n (hide n (lambda () (equal? x y))))))


(define (equality-benchmarks n0 n1 n2 n3 n4 n5)
  (and 
       (equality-benchmark1 n1)
       ))

(define (main)
  (let* ((input0 100)
         (input1 50) ;100)
         (input2 8)
         (input3 1000)
         (input4 2000)
         (input5 5000)
         (output #t)
         (s5 (number->string input5))
         (s4 (number->string input4))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (s0 (number->string input0))
         (name "equal"))
    (run-r7rs-benchmark
     (string-append name ":" s0 ":" s1 ":" s2 ":" s3 ":" s4 ":" s5)
     1
     (lambda ()
       (equality-benchmarks (hide input0 input0)
                            (hide input0 input1)
                            (hide input0 input2)
                            (hide input0 input3)
                            (hide input0 input4)
                            (hide input0 input5)))
     (lambda (result) (eq? result #t)))))

;;; The following code is appended to all benchmarks.

;;; Given an integer and an object, returns the object
;;; without making it too easy for compilers to tell
;;; the object will be returned.

(define (hide r x)
  (call-with-values
   (lambda ()
     (values (vector values (lambda (x) x))
             (if (< r 100) 0 1)))
   (lambda (v i)
     ((vector-ref v i) x))))

;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations.

(define (run-r7rs-benchmark name count thunk ok?)

  ;; Rounds to thousandths.
  (define (rounded x)
    (/ (round (* 1000 x)) 1000))

  (display "Running ")
  (display name)
  (newline)
  (flush-output-port)
  (let* ((j/s (jiffies-per-second))
         (t0 (current-second))
         (j0 (current-jiffy)))
    (let loop ((i 0)
               (result (if #f #f)))
      (cond ((< i count)
             (loop (+ i 1) (thunk)))
            ((ok? result)
             (let* ((j1 (current-jiffy))
                    (t1 (current-second))
                    (jifs (- j1 j0))
                    (secs (inexact (/ jifs j/s)))
                    (secs2 (rounded (- t1 t0))))
               (display "Elapsed time: ")
               (write secs)
               (display " seconds (")
               (write secs2)
               (display ") for ")
               (display name)
               (newline))
             result)
            (else
             (display "ERROR: returned incorrect result: ")
             (write result)
             (newline)
             result)))))

(main)
