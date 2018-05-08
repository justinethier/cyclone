;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the complex library from r7rs.
;;;;
(define-library (scheme complex)
  (export
    angle 
    imag-part
    magnitude 
    make-polar
    make-rectangular 
    real-part
  )
  (inline
    real-part
    imag-part)
  (import (scheme base)
          (scheme inexact))
  (begin
    (define (real-part x) 
      (if (complex? x)
          (%real-part x)
           x))

    (define (imag-part x) 
      (if (complex? x)
          (%imag-part x)
          0))

    (define (magnitude z)
      (sqrt (+ (* (real-part z) (real-part z))
               (* (imag-part z) (imag-part z)))))

    (define (angle z) (atan (imag-part z) (real-part z)))

    (define (make-polar r phi)
      (make-rectangular (* r (cos phi)) (* r (sin phi))))

    (define (make-rectangular x y)
      (%make-rect x y))

    (define-c %real-part
      "(void *data, int argc, closure _, object k, object z)"
      " make_double(d, creal(complex_num_value(z)));
        return_closcall1(data, k, &d); ")

    (define-c %imag-part
      "(void *data, int argc, closure _, object k, object z)"
      " make_double(d, cimag(complex_num_value(z)));
        return_closcall1(data, k, &d); ")

    (define-c %make-rect
      "(void *data, int argc, closure _, object k, object r, object i)"
      " Cyc_check_num(data, r);
        Cyc_check_num(data, i);
        Cyc_make_rectangular(data, k, r, i); ")
))
