;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2007 William D Clinger.
;;
;; Permission to copy this software, in whole or in part, to use this
;; software for any lawful purpose, and to redistribute this software
;; is granted subject to the restriction that all copies made of this
;; software must include this copyright notice in full.
;;
;; I also request that you send me a copy of any improvements that you
;; make to this software so that they may be incorporated within it to
;; the benefit of the Scheme community.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests of string <-> bytevector conversions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base) (scheme read) (scheme write) (scheme time))

;; Crude test rig, just for benchmarking.

(define failed-tests '())

(define (test name actual expected)
  (unless (equal? actual expected)
    (display "******** FAILED TEST ******** ")
    (display name)
    (newline)
    (set! failed-tests (cons name failed-tests))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We're limited to Ascii strings here because the R7RS doesn't
;; actually require anything beyond Ascii.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic sanity tests, followed by stress tests on random inputs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-bytevector-tests
         *random-stress-tests* *random-stress-test-max-size*)

  (define (test-roundtrip bvec tostring tobvec)
    (let* ((s1 (tostring bvec))
           (b2 (tobvec s1))
           (s2 (tostring b2)))
      (test "round trip of string conversion" (string=? s1 s2) #t)))

  ;; This random number generator doesn't have to be good.
  ;; It just has to be fast.

  (define random
    (letrec ((random14
              (lambda (n)
                (set! x (remainder (+ (* a x) c) (+ m 1)))
                (remainder (quotient x 8) n)))
             (a 701)
             (x 1)
             (c 743483)
             (m 524287)
             (loop
              (lambda (q r n)
                (if (zero? q)
                    (remainder r n)
                    (loop (quotient q 16384)
                          (+ (* 16384 r) (random14 16384))
                          n)))))
      (lambda (n)
        (if (< n 16384)
            (random14 n)
            (loop (quotient n 16384) (random14 16384) n)))))

  ;; Returns a random bytevector of length up to n,
  ;; with all elements less than 128.

  (define (random-bytevector n)
    (let* ((n (random n))
           (bv (make-bytevector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) bv)
        (bytevector-u8-set! bv i (random 128)))))

  ;; Returns a random bytevector of even length up to n.

  (define (random-bytevector2 n)
    (let* ((n (random n))
           (n (if (odd? n) (+ n 1) n))
           (bv (make-bytevector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) bv)
        (bytevector-u8-set! bv i (random 128)))))

  ;; Returns a random bytevector of multiple-of-4 length up to n.

  (define (random-bytevector4 n)
    (let* ((n (random n))
           (n (* 4 (round (/ n 4))))
           (bv (make-bytevector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) bv)
        (bytevector-u8-set! bv i (random 128)))))

  (test-roundtrip (random-bytevector 10) utf8->string string->utf8)

  (do ((i 0 (+ i 1)))
      ((= i *random-stress-tests*))
    (test-roundtrip (random-bytevector *random-stress-test-max-size*)
                    utf8->string string->utf8))

  )

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "bv2string"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda ()
       (string-bytevector-tests (hide count input1) (hide count input2))
       (length failed-tests))
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

(import (srfi 18) (scheme process-context))

(define *running-threads* '())
(define m (make-mutex))

(define (async-exec-multi! n thunk)
  (do ((i 0 (+ i 1)))
      ((>= i n)) 
    (async-exec! thunk)))

(define (async-exec! thunk)
  (let ((t (make-thread
             (lambda ()
               (mutex-lock! m)
               (let ((t (cons (current-thread) *running-threads*)))
                (cond-expand
                  (cyclone (Cyc-minor-gc))) ;; Move t to heap
                (set! *running-threads* t))
               (mutex-unlock! m)
               (thunk))))) 
    (thread-start! t)))

(define (wait-for-all-async)
  (thread-sleep! 0) ;; TODO: not good enough, figure out a better solution
  (let loop ()
    (define t #f)
    (mutex-lock! m)
    (when (not (null? *running-threads*))
      (set! t (car *running-threads*))
      (set! *running-threads* (cdr *running-threads*)))
    (mutex-unlock! m)

    (when t
      (thread-join! t)
      (loop))))

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
   (async-exec-multi! 2 (lambda () 
    (let loop ((i 0)
               (result #f))
      (cond ((< i count)
             (loop (+ i 1) (thunk)))
            ((ok? result)
             (display "Thread finished with correct result")
             (newline)
             result)
            (else
             (display "ERROR: returned incorrect result: ")
             (write result)
             (newline)
             (flush-output-port (current-output-port))
             (exit 1)
             result)))
    ))
    (wait-for-all-async)
    
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
    ))
(define (this-scheme-implementation-name)
  (string-append "cyclone-" #;(Cyc-version)))
(main)
