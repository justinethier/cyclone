;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the lazy library from r7rs.
;;;;
(define-library (scheme lazy)
  (import (scheme base))
  (export 
    delay 
    force 
    delay-force 
    make-promise 
    promise?
  )
  (begin

  (define force
      (lambda (object)
        (object)))

  (define-syntax delay
    (er-macro-transformer
     (lambda (expr rename compare)
       `(make-promise (lambda () ,(cadr expr))))))

  (define-syntax delay-force
    (er-macro-transformer
     (lambda (expr rename compare)
       `(make-promise (lambda () ,(cadr expr))))))

  (define make-promise
    (lambda (proc)
      (let ((result-ready? #f)
            (result #f))
        (lambda ()
          (if result-ready? 
              result
              (let ((x (proc)))
                (if result-ready?
                    result
                    (begin (set! result x)
                           (set! result-ready? #t)
                           result))))))))

  ;; Not a very satisfying implementation, but would need to change
  ;; how promises are stored to do better
  (define promise? procedure?)
))
