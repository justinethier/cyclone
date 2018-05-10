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
  (import (scheme base))
  (begin
    (define (real-part x) x)
    (define (imag-part x) 0)
    (define (angle z) 
      (error "Complex numbers are not supported at this time"))
    (define (magnitude z) 
      (error "Complex numbers are not supported at this time"))
    (define (make-rectangular x y)
      (Cyc-make-rect x y))
    (define (make-polar x y)
      (error "Complex numbers are not supported at this time"))

    (define-c Cyc-make-rect
      "(void *data, int argc, closure _, object k, object r, object i)"
      " Cyc_check_num(data, r);
        Cyc_check_num(data, i);
        Cyc_make_rectangular(data, k, r, i); ")
))
