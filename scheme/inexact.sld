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
                 (op (caddr expr)))
           `(define-c ,fnc
              "(void *data, int argc, closure _, object k, object z)"
              ,(string-append
                " return_inexact_double_op(data, k, " op ", z);")
              "(void *data, object ptr, object z)"
              ,(string-append
                " return_inexact_double_op_no_cps(data, ptr, " op ", z);"))))))

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
    (define-inexact-op c-log "log")
    (define-inexact-op exp "exp")
    (define-inexact-op sqrt "sqrt")
    (define-inexact-op sin "sin")
    (define-inexact-op cos "cos")
    (define-inexact-op tan "tan")
    (define-inexact-op asin "asin")
    (define-inexact-op acos "acos")
    (define-inexact-op atan "atan")
))
