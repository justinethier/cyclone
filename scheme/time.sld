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
      " make_double(box, 0.0);
        clock_t jiffy = clock();
        double_value(&box) = jiffy;
        return_closcall1(data, k, &box); ")
    (define-c jiffies-per-second
      "(void *data, int argc, closure _, object k)"
      " make_int(box, CLOCKS_PER_SEC);
        return_closcall1(data, k, &box); ")
  ))
