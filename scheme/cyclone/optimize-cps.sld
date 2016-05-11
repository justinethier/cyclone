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
          (scheme cyclone util)
          (scheme cyclone ast)
          (scheme cyclone optimize-cps)
          (scheme cyclone transforms)
          (srfi 69))
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
    (define (analyze-cps exp)
      (define (analyze exp lid)
        (cond
          ; Core forms:
          ((ast:lambda? exp)
           (let ((id (ast:lambda-id exp)))
             ;; save lambda to adb
             (adb:set!
               id
               (adb:make-fnc)) ;; TODO: anything to record???? params?
             (for-each
               (lambda (expr)
                 (analyze expr id))
               (ast:lambda-body))))
;TODO:          ((ref? exp)      (if (and (not (member exp globals))
;TODO:                                    (is-mutable? exp))
;TODO:                               `(cell-get ,exp)
;TODO:                               exp))
;TODO:          ((set!? exp)     `(,(if (member (set!->var exp) globals)
;TODO:                                  'set-global!
;TODO:                                  'set-cell!) 
;TODO:                              ,(set!->var exp) 
;TODO:                              ,(wrap-mutables (set!->exp exp) globals)))
          ((if? exp)       `(if ,(analyze (if->condition exp) lid)
                                ,(analyze (if->then exp) lid)
                                ,(analyze (if->else exp) lid)))
          
          ; Application:
          ((app? exp)
           (map (lambda (e)
                  (analyze-cps e lid))
                exp))
;TODO:          ((app? exp)      (map (lambda (e) (wrap-mutables e globals)) exp))

          ; Nothing to analyze for these?
          ;((prim? exp)     exp)
          ;((quote? exp)    exp)
          ; Should never see vanilla lambda's in this function, only AST's
          ;((lambda? exp)
          ;; Nothing to analyze for expressions that fall into this branch
          (else
            #f)))
      (analyze exp -1) ;; Top-level is lambda ID -1
    )
))
