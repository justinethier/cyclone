;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the write library from r7rs.
;;;;
(define-library (scheme write)
  (export
    display
    write
    write-shared
    write-simple)
  (import (scheme base))
  (begin
    (define (display obj . port)
      (if (null? port)
          (Cyc-display obj (current-output-port))
          (Cyc-display obj (car port))))
    (define (write obj . port)
      (if (null? port)
          (Cyc-write obj (current-output-port))
          (Cyc-write obj (car port))))
    (define write-shared write)
    (define write-simple write)
  )
)
