;;; CTAK -- A version of the TAK procedure that uses continuations.

(import (scheme base) (scheme read) (scheme write) (scheme time) (srfi 18))

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
       (lambda (k)
         (ctak-aux
          k
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- x 1) y z)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- y 1) z x)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "ctak"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       #;(thread-start!
         (make-thread
           (lambda ()
             (ctak (hide count input1) (hide count input2) (hide count input3)))))
       #;(thread-start!
         (make-thread
           (lambda ()
             (ctak (hide count input1) (hide count input2) (hide count input3)))))
       #;(thread-start!
         (make-thread
           (lambda ()
             (ctak (hide count input1) (hide count input2) (hide count input3)))))
       (thread-start!
         (make-thread
           (lambda ()
             (ctak (hide count input1) (hide count input2) (hide count input3)))))
       (thread-sleep! 10000)
       (ctak (hide count input1) (hide count input2) (hide count input3))

       ;; TODO: thread-join
     )
     (lambda (result) (equal? result output)))))

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
  (flush-output-port (current-output-port))
  (let* ((j/s (jiffies-per-second))
         (t0 (current-second))
         (j0 (current-jiffy)))
    (let loop ((i 0)
               (result #f))
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
               (newline)
               (display "+!CSVLINE!+")
               (display (this-scheme-implementation-name))
               (display ",")
               (display name)
               (display ",")
               (display secs)
               (newline)
               (flush-output-port (current-output-port)))
             result)
            (else
             (display "ERROR: returned incorrect result: ")
             (write result)
             (newline)
             (flush-output-port (current-output-port))
             result)))))
(define (this-scheme-implementation-name)
  (string-append "cyclone-" (Cyc-version)))
(main)
