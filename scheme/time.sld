;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the time library from r7rs.
;;;;
(define-library (scheme time)
  (export 
    current-second
    current-jiffy
    jiffies-per-second
  )
  (include-c-header "<sys/time.h>")
  (import (scheme base) 
  )
;; TODO: get an FFI syntax for including C header files, even if it is not needed for this library
  (begin
    (define-c current-second
      "(void *data, int argc, closure _, object k)"
      " make_double(box, 0.0);
        time_t t = time(NULL);
        double_value(&box) = t;
        return_closcall1(data, k, &box); ")
    (define-c current-jiffy
      "(void *data, int argc, closure _, object k)"
      " struct timeval tv;
        make_double(box, 0.0);
        gettimeofday(&tv, NULL); /* TODO: longer-term consider using clock_gettime instead */
        long long jiffy = (tv.tv_sec)*1000000LL + tv.tv_usec;
        /* Future consideration:
        mp_int bn_tmp, bn_tmp2, bn_tmp3;
        mp_init(&bn_tmp);
        mp_init(&bn_tmp2);
        mp_init(&bn_tmp3);
        Cyc_int2bignum(tv.tv_sec, &bn_tmp);
        Cyc_int2bignum(1000000LL, &bn_tmp2);
        Cyc_int2bignum(tv.tv_usec, &bn_tmp3);
        alloc_bignum(data, box);
        mp_mul(&bn_tmp, &bn_tmp2, &bn_tmp);
        mp_add(&bn_tmp, &bn_tmp3, &bignum_value(box)); 
        mp_clear(&bn_tmp);
        mp_clear(&bn_tmp2);
        mp_clear(&bn_tmp3);
        */
        double_value(&box) = jiffy;
        return_closcall1(data, k, &box); ")
    (define-c jiffies-per-second
      "(void *data, int argc, closure _, object k)"
      " int n = 1000000;
        object obj = obj_int2obj(n);
        return_closcall1(data, k, obj); ")
  ))
