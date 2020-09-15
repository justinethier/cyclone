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

  ;; promise
  ;; ( tag value/obj )

  (define *promise-tag* '(promise))
  (define (promise? obj)
    (and (pair? obj)
         (eq? *promise-tag* (car obj))))

  (define force
      (lambda (obj)
        (if (promise? obj)
            (force ((cdr obj)))
            obj)))

  (define-syntax delay
    (er-macro-transformer
     (lambda (expr rename compare)
       `(make-promise (lambda () ,(cadr expr))))))

  (define-syntax delay-force
    (er-macro-transformer
     (lambda (expr rename compare)
       `(make-promise (lambda () ,(cadr expr))))))

  (define make-promise
    (lambda (obj)
      (if (promise? obj)
          obj
          (let ((result-ready? #f)
                (result #f))
            (cons
              *promise-tag*
              (lambda ()
                (if result-ready? 
                    result
                    (let ((x (if (procedure? obj)
                                 (obj)
                                 obj)))
                      (if result-ready?
                          result
                          (begin (set! result x)
                                 (set! result-ready? #t)
                                 result))))))))))

))
