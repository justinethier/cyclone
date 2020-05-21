;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the inexact library from r7rs.
;;;;
(define-library (scheme inexact)
  (import (scheme base))
  (export 
    acos 
    asin
    atan 
    cos
    exp 
    finite?
    infinite? 
    log
    nan? 
    sin
    sqrt 
    tan
  )
  (begin
    (define-syntax define-inexact-op
      (er-macro-transformer
        (lambda (expr rename compare)
          (let* ((fnc (cadr expr))
                 (op (caddr expr))
                 (complex-op (cadddr expr))
                )
           `(define-c ,fnc
              "(void *data, int argc, closure _, object k, object z)"
              ,(string-append
                " return_inexact_double_or_cplx_op(data, k, " op ", " complex-op ", z);")
              "(void *data, object ptr, object z)"
              ,(string-append
                " return_inexact_double_or_cplx_op_no_cps(data, ptr, " op ", " complex-op ", z);"))))))

    (define-c nan?
      "(void *data, int argc, closure _, object k, object z)"
      " Cyc_check_num(data, z);
        if (obj_is_int(z) || 
            type_of(z) == integer_tag ||
            type_of(z) == bignum_tag ||
            !isnan(((double_type *)z)->value)) 
        {
          return_closcall1(data, k, boolean_f);
        }
        return_closcall1(data, k, boolean_t);")
    (define-c infinite?
      "(void *data, int argc, closure _, object k, object z)"
      " Cyc_check_num(data, z);
        if (obj_is_int(z) || 
            type_of(z) == integer_tag ||
            type_of(z) == bignum_tag ||
            !isinf(((double_type *)z)->value)) 
        {
          return_closcall1(data, k, boolean_f);
        }
        return_closcall1(data, k, boolean_t);")
    (define (finite? z)
      (if (infinite? z) #f #t))
    (define (log z1 . z2)
      (if (null? z2)
          (c-log z1)
          (let ((z2* (car z2)))
            (/ (c-log z1) (c-log z2*)))))
    (define-inexact-op c-log "log"   "clog")
    (define-inexact-op exp   "exp"   "cexp")
    (define-inexact-op sqrt  "sqrt"  "csqrt")
    (define-inexact-op sin   "sin"   "csin")
    (define-inexact-op cos   "cos"   "ccos")
    (define-inexact-op tan   "tan"   "ctan")
    (define-inexact-op asin  "asin"  "casin")
    (define-inexact-op acos  "acos"  "cacos")
    (define-inexact-op atan1 "atan"  "catan")

    ;; Support for two-argument atan, from Chibi Scheme
    (define (atan y . o)
      (define (inf? z) (if (= +inf.0 z) #t (= -inf.0 z)))
      (if (null? o)
          (atan1 y)
          (let ((x (inexact (car o))))
            (if (and (inf? x) (inf? y))
                (* (if (< y 0) -1 1) (if (= x -inf.0) 3 1) 0.7853981633974483)
                (if (negative? x)
                    (if (or (negative? y) (eqv? y -0.0))
                        (- (atan1 (/ y x)) 3.141592653589793)
                        (- 3.141592653589793 (atan1 (/ y (- x)))))
                    (if (and (zero? x) (zero? y))
                        (* (if (eqv? y -0.0) -1 1)
                           (if (eqv? x -0.0) 3.141592653589793 x))
                        (atan1 (/ y x))))))))
))
