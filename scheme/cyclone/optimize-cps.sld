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
          (scheme cyclone transforms)
          (srfi 69))
  (export
      analyze-cps
      ;adb:init!
      adb:get
      adb:get/default
      adb:set!
      adb:get-db
      simple-lambda?
      ;; Analyze variables
      adb:make-var
      %adb:make-var
      adb:variable?
      adbv:global 
      adbv:set-global!
      adbv:defined-by adbv:set-defined-by!
      adbv:assigned adbv:set-assigned!
      adbv:assigned-locally adbv:set-assigned-locally!
      adbv:ref-by
      adbv:set-ref-by!
      ;; Analyze functions
      adb:make-fnc
      %adb:make-fnc
      adb:function?
      adbf:simple adbf:set-simple!
      adbf:unused-params adbf:set-unused-params!
  )
  (begin
    (define *adb* (make-hash-table))
    (define (adb:get-db) *adb*)
    ;(define *adb* #f) ;(make-hash-table))
    ;(define (adb:init!)
    ;  ;(set! *adb* (make-hash-table)))
    ;  'TODO)
    (define (adb:get key) (hash-table-ref *adb* key))
    (define (adb:get/default key default) (hash-table-ref/default *adb* key default))
    (define (adb:set! key val) (hash-table-set! *adb* key val))
    (define-record-type <analysis-db-variable>
      (%adb:make-var global defined-by assigned assigned-locally)
      adb:variable?
      (global adbv:global adbv:set-global!)
      (defined-by adbv:defined-by adbv:set-defined-by!)
      (assigned adbv:assigned adbv:set-assigned!)
      (assigned-locally adbv:assigned-locally adbv:set-assigned-locally!)
      (ref-by adbv:ref-by adbv:set-ref-by!)
    )
    (define (adb:make-var)
      (%adb:make-var '? '? '? '? '()))

    (define-record-type <analysis-db-function>
      (%adb:make-fnc simple unused-params)
      adb:function?
      (simple adbf:simple adbf:set-simple!)
      (unused-params adbf:unused-params adbf:set-unused-params!)
    )
    (define (adb:make-fnc)
      (%adb:make-fnc '? '?))

    (define (analyze-cps exp)
      (define (analyze exp lid)
;(trace:error `(analyze ,lid ,exp))
        (cond
          ; Core forms:
          ((ast:lambda? exp)
           (let ((id (ast:lambda-id exp))
                  (fnc (adb:make-fnc)))
             ;; save lambda to adb
             (adb:set! id fnc)
             ;; Analyze the lambda
             (for-each
              (lambda (arg)
                (let ((var (adb:get/default arg (adb:make-var))))
                  (adbv:set-global! var #f)
                  (adbv:set-defined-by! var id)
                  (adb:set! arg var)))
              (ast:lambda-formals->list exp))
             (for-each
               (lambda (expr)
                 (analyze expr id))
               (ast:lambda-body exp))))
          ((ref? exp)
           (let ((var (adb:get/default exp (adb:make-var))))
            (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))
           ))
          ((define? exp)
           (let ((var (adb:get/default (define->var exp) (adb:make-var))))
             ;; TODO:
             (adbv:set-defined-by! var lid)
             (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))

             (analyze (define->exp exp) lid)))
          ((set!? exp)
           (let ((var (adb:get/default (set!->var exp) (adb:make-var))))
             ;; TODO:
             (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))

             (analyze (set!->exp exp) lid)))
          ((if? exp)       `(if ,(analyze (if->condition exp) lid)
                                ,(analyze (if->then exp) lid)
                                ,(analyze (if->else exp) lid)))
          
          ; Application:
          ((app? exp)
           (map (lambda (e)
                  (analyze e lid))
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

    ;; TODO: make another pass for simple lambda's
    ;can use similar logic to cps-optimize-01:
    ;- body is a lambda app
    ;- no lambda args are referenced in the body of that lambda app
    ;  (ref-by is empty or the defining lid)
    ;
    ; Need to check analysis DB against CPS generated and make sure
    ; things like ref-by make sense (ref by seems like its only -1 right now??)
    (define (simple-lambda? exp)
      #f)
))
