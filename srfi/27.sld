;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright 2016 Jason K. MacDuffie
;;;; License: MIT (Expat) License
;;;;
;;;; Right now this uses an LCG, and should probably be replaced
;;;; with a higher-quality implementation as soon as possible.
;;;; For casual use this will work OK.

(define-library (srfi 27)
  (import (scheme base)
          (scheme case-lambda)
          (scheme time))
  (export random-integer random-real default-random-source
          next-mrg32k3a ;; TODO: only here for testing
          make-random-source 
          ; random-source?
          random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals)
  (begin
    ;; Numbers taken from bsd random
    ;(define mult 1103515245)
    (define incr (exact (current-second)))
    (define m 536870912)
    ;; Cutting off seems like a good idea
    ;(define cutoff 100)

    (define-c next-lcg
      "(void *data, int argc, closure _, object k, object seed)"
      "object result = NULL;
       unsigned int s = obj_obj2int(seed);
       unsigned int mult = 1103515245;
       unsigned int incr = 12345;
       unsigned int m = 536870912;
       unsigned int next_seed = ((s * mult) + incr) % m;
       result = obj_int2obj(next_seed);
       return_closcall1(data, k, result);")

    ;; Testing this out
    ;; TODO: handle ints, too. of course that also adds overhead...
    (define-c next-mrg32k3a
      "(void *data, int argc, closure _, object k, object seed)"
      "double dval = MRG32k3a( double_value(seed) );
       {
        make_double(result, dval);
        return_closcall1(data, k, &result);
       }")

    (define-record-type <random-source>
      (raw-random-source n)
      random-souce?
      (n random-source-val set-random-source-val!))
    
    (define (make-random-source)
      (raw-random-source incr))
    
    (define (random-source-make-integers s)
      (lambda (n)
        (define nextval
          (next-lcg (random-source-val s)))
        (set-random-source-val! s nextval)
        (modulo nextval n)))
    
    (define random-source-make-reals
      (case-lambda
        ((s)
         (random-source-make-reals s (/ 1.0 m)))
        ((s unit)
         (if (not (< 0.0 unit 1.0))
             (error "unit must be between 0.0 and 1.0 (exclusive), but got " unit))
         (let ((numgen (random-source-make-integers s)))
           (define r (exact (floor (/ 1.0 unit))))
           (lambda ()
             (* (numgen r) unit))))))
    
    (define default-random-source (make-random-source))
    
    (define random-integer
      (random-source-make-integers default-random-source))
    
    (define random-real
      (random-source-make-reals default-random-source))
    
    (define (random-source-state-ref s)
      ;; Just return the integer
      (random-source-val s))
    
    (define (random-source-state-set! s state)
      ;; Just set the integer
      (set-random-source-val! s state))
    
    (define (random-source-pseudo-randomize! s i j)
      ;; Pretty bad quality, upgrade after switch to better gen
      (define n1
        i) ;((random-source-make-integers i) m))
      (define n2
        j) ;((random-source-make-integers j) m))
      (define n3
        (abs (+ n1 n2)))
      (random-source-state-set! s n3))
    
    (define (random-source-randomize! s)
      ;; True randomness would be a good idea here
      (define i (current-second))
      (define j (+ (current-second) 2))
      (random-source-pseudo-randomize! s i j))))

