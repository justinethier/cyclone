;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; A stub for the cxr library from r7rs.
;;;; At least for now, the actual cxr functions are primitives in the runtime, 
;;;; so this library does not need to be imported to use them.
;;;;
(define-library (scheme cxr)
  (export caaaaar)
  (begin 
    ;; Non-standard, this is just a placeholder
    (define (caaaaar lis)
      (car (car (car (car (car lis))))))))
