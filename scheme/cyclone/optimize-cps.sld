;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module performs CPS analysis and optimizations.
;;;;

;; TODO:
;- add 'analyze' function, can base it on expand or another transform to start
;- modify cps to use ast for lambda's
;  will need analyze to use the ast, and will need
;  closure conversion to recognize it, too.
;  at least for now, closure conversion can output 
;  regular lambda's, though.
;  can write initial analyze, but can't get too far without being able
;  to uniquely ID each lambda

;(define-library (optimize-cps)
(define-library (scheme cyclone optimize-cps)
  (import (scheme base)
          (srfi 69)
          ;(scheme char)
          ;(scheme read)
          ;(scheme write)
          ;(scheme cyclone common)
          ;(scheme cyclone libraries)
          ;(scheme cyclone macros)
          ;(scheme cyclone pretty-print)
          ;(scheme cyclone util)
          ;(scheme cyclone transforms)
  )
  (export
      analyze-cps
      ;adb:init!
      adb:get key
      adb:set! key val
      ;; Variables
      adb:make-var
      %adb:make-var
      adb:variable?
      adbv:global 
      adbv:set-global!
      adbv:defined-by adbv:set-defined-by!
      adbv:assigned adbv:set-assigned!
      adbv:assigned-locally adbv:set-assigned-locally!
      ;; Functions
      adb:make-fnc
      %adb:make-fnc
      adb:function?
      adbf:simple adbf:set-simple!
      adbf:unused-params adbf:set-unused-params!
  )
  (begin
    (define *adb* (make-hash-table))
    ;(define *adb* #f) ;(make-hash-table))
    ;(define (adb:init!)
    ;  ;(set! *adb* (make-hash-table)))
    ;  'TODO)
    (define (adb:get key) (hash-table-ref *adb* key))
    (define (adb:set! key val) (hash-table-set! *adb* key val))
    (define-record-type <analysis-db-variable>
      (%adb:make-var global defined-by assigned assigned-locally)
      adb:variable?
      (global adbv:global adbv:set-global!)
      (defined-by adbv:defined-by adbv:set-defined-by!)
      (assigned adbv:assigned adbv:set-assigned!)
      (assigned-locally adbv:assigned-locally adbv:set-assigned-locally!)
    )
    (define (adb:make-var)
      (%adb:make-var #f #f #f #f))

    (define-record-type <analysis-db-function>
      (%adb:make-fnc simple unused-params)
      adb:function?
      (simple adbf:simple adbf:set-simple!)
      (unused-params adbf:unused-params adbf:set-unused-params!)
    )
    (define (adb:make-fnc)
      (%adb:make-fnc #f #f))

; TODO: analyze-cps
;    (define (wrap-mutables exp globals)
;    
;    (define (wrap-mutable-formals formals body-exp)
;      (if (not (pair? formals))
;          body-exp
;          (if (is-mutable? (car formals))
;              `((lambda (,(car formals))
;                  ,(wrap-mutable-formals (cdr formals) body-exp))
;                (cell ,(car formals)))
;              (wrap-mutable-formals (cdr formals) body-exp))))
;    
;    (cond
;      ; Core forms:
;      ((ast:lambda? exp)
;       `(lambda ,(ast:lambda-args exp)
;         ,(wrap-mutable-formals 
;           (ast:lambda-formals->list exp)
;           (wrap-mutables (car (ast:lambda-body exp)) globals)))) ;; Assume single expr in lambda body, since after CPS phase
;      ((const? exp)    exp)
;      ((ref? exp)      (if (and (not (member exp globals))
;                                (is-mutable? exp))
;                           `(cell-get ,exp)
;                           exp))
;      ((prim? exp)     exp)
;      ((quote? exp)    exp)
;      ((lambda? exp)   `(lambda ,(lambda->formals exp)
;                          ,(wrap-mutable-formals (lambda-formals->list exp)
;                                                 (wrap-mutables (car (lambda->exp exp)) globals)))) ;; Assume single expr in lambda body, since after CPS phase
;      ((set!? exp)     `(,(if (member (set!->var exp) globals)
;                              'set-global!
;                              'set-cell!) 
;                          ,(set!->var exp) 
;                          ,(wrap-mutables (set!->exp exp) globals)))
;      ((if? exp)       `(if ,(wrap-mutables (if->condition exp) globals)
;                            ,(wrap-mutables (if->then exp) globals)
;                            ,(wrap-mutables (if->else exp) globals)))
;      
;      ; Application:
;      ((app? exp)      (map (lambda (e) (wrap-mutables e globals)) exp))
;      (else            (error "unknown expression type: " exp))))
))
