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
     ast:get-next-lambda-id!
     ast:reset-lambda-ids!
     ast:ast->pp-sexp
     ast:ast->sexp
     ast:sexp->ast
  )
  (begin
    (define ast:ast->sexp ast->sexp)

    (define ast:sexp->ast sexp->ast)

    (define *lambda-id* 0)

    (define (ast:get-next-lambda-id!)
      (set! *lambda-id* (+ 1 *lambda-id*))
      *lambda-id*)

    (define (ast:reset-lambda-ids!)
      (set! *lambda-id* 0))

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

    ;; Transform a SEXP in AST form to one that prints more cleanly
    (define (ast:ast->pp-sexp exp)
      (cond
       ((ast:lambda? exp)
        (let* ((id (ast:lambda-id exp))
               (has-cont (ast:lambda-has-cont exp))
               (sym (string->symbol 
                      (string-append
                         "lambda-"
                         (number->string id)
                         (if has-cont "-cont" ""))))
              )
          `(,sym ,(ast:lambda-args exp)
             ,@(map ast:ast->pp-sexp (ast:lambda-body exp))))
       )
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) exp)
       ((define? exp)
        `(define ,(define->var exp)
                 ,@(ast:ast->pp-sexp (define->exp exp))))
       ((set!? exp)
        `(set! ,(set!->var exp)
               ,(ast:ast->pp-sexp (set!->exp exp))))
       ((if? exp)       
        `(if ,(ast:ast->pp-sexp (if->condition exp))
             ,(ast:ast->pp-sexp (if->then exp))
             ,(ast:ast->pp-sexp (if->else exp))))
       ((app? exp)
        (map ast:ast->pp-sexp exp))
       (else exp)))

    ;; Transform AST back to an equivalent sexp
    (define (ast->sexp exp)
      (cond
       ((ast:lambda? exp)
        (let* ((id (ast:lambda-id exp))
               (has-cont (ast:lambda-has-cont exp))
               (sym 'lambda))
          `(,sym ,(ast:lambda-args exp)
             ,@(map ast->sexp (ast:lambda-body exp)))))
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) exp)
       ((define? exp)
        `(define ,(define->var exp)
                 ,@(ast->sexp (define->exp exp))))
       ((set!? exp)
        `(set! ,(set!->var exp)
               ,(ast->sexp (set!->exp exp))))
       ((if? exp)       
        `(if ,(ast->sexp (if->condition exp))
             ,(ast->sexp (if->then exp))
             ,(ast->sexp (if->else exp))))
       ((app? exp)
        (map ast->sexp exp))
       (else exp)))

    ;; Transform given SEXP into AST form.
    (define (sexp->ast exp)
      (cond
       ((lambda? exp)
        (ast:make-lambda 
          (lambda->formals exp)
          (map sexp->ast (lambda->exp exp))))
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) exp)
       ((define? exp)
        `(define ,(define->var exp)
                 ,@(sexp->ast (define->exp exp))))
       ((set!? exp)
        `(set! ,(set!->var exp)
               ,(sexp->ast (set!->exp exp))))
       ((if? exp)       
        `(if ,(sexp->ast (if->condition exp))
             ,(sexp->ast (if->then exp))
             ,(sexp->ast (if->else exp))))
       ((app? exp)
        (map sexp->ast exp))
       (else exp)))
))
