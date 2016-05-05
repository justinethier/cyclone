;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module defines abstract syntax tree types used during compilation.
;;;;

(define-library (ast)
;(define-library (scheme cyclone ast)
  (import (scheme base))
  (export
     ast:make-lambda
     ast:lambda?
     ast:lambda-id
     ast:lambda-args 
     ast:set-lambda-args!
     ast:lambda-body 
     ast:se-lambda-body!
  )
  (begin
    (define-record-type <lambda-ast>
     (ast:make-lambda id args body)
     ast:lambda?
     (id ast:lambda-id)
     (args ast:lambda-args ast:set-lambda-args!)
     (body ast:lambda-body ast:se-lambda-body!))
))
