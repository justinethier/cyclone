;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module defines abstract syntax tree types used during compilation.
;;;;

;(define-library (ast)
(define-library (scheme cyclone ast)
  (import (scheme base)
          (scheme cyclone util)
  )
  (export
     ast:make-lambda
     ast:%make-lambda
     ast:lambda?
     ast:lambda-id
     ast:lambda-args 
     ast:set-lambda-args!
     ast:lambda-body 
     ast:set-lambda-body!
     ast:lambda-has-cont 
     ast:set-lambda-has-cont!
  )
  (begin
    (define *lambda-id* 0)
    (define-record-type <lambda-ast>
     (ast:%make-lambda id args body has-cont)
     ast:lambda?
     (id ast:lambda-id)
     (args ast:lambda-args ast:set-lambda-args!)
     (body ast:lambda-body ast:set-lambda-body!)
     (has-cont ast:lambda-has-cont ast:set-lambda-has-cont!)
    )
    (define (ast:make-lambda args body . opts)
      (let ((has-cont (if (pair? opts) (car opts) #f)))
        (set! *lambda-id* (+ 1 *lambda-id*))
        (ast:%make-lambda *lambda-id* args body has-cont)))
))
