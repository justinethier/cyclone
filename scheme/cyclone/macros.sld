;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains code to deal with macros.
;;;;
(define-library (scheme cyclone macros)
  (import (scheme base)
          (scheme write) ;; Debug only
          (scheme eval)
          (scheme cyclone util)
  )
  (export
    define-syntax?
    macro:macro?
    macro:expand
    macro:add!
    macro:cleanup
    macro:load-env!
    macro:get-env
    macro:get-defined-macros
  )
  (begin
    ;; top-level macro environment
    (define *macro:env* '())

    ;; A list of all macros defined by the program/library being compiled
    (define *macro:defined-macros* '())

    (define *macro:renamed-variables* (env:extend-environment '() '() '()))

    (define (macro:add! name body)
      (set! *macro:defined-macros* 
        (cons (cons name body) *macro:defined-macros*))
      #t)

    (define (macro:load-env! defined-macros base-env)
      (set! *macro:env* (env:extend-environment
                          (map car defined-macros)
                          (map (lambda (v)
                                 (list 'macro (cdr v)))
                               defined-macros)
                          base-env)))

    (define (macro:get-env) *macro:env*)

    (define (macro:get-defined-macros) *macro:defined-macros*)

    ;; Macro section
    (define (define-syntax? exp)
      (tagged-list? 'define-syntax exp))

    (define (macro:macro? exp defined-macros) (assoc (car exp) defined-macros))

    (define (macro:expand exp macro mac-env)
      (let* ((use-env (env:extend-environment '() '() '()))
             (compiled-macro? (or (Cyc-macro? (Cyc-get-cvar (cadr macro)))
                                  (procedure? (cadr macro))))
             (result #f))
        ;(newline)
        ;(display "/* ")
        ;(display (list 'macro:expand exp macro compiled-macro?))
        ;(display "*/ ")

          ;; Invoke ER macro
        (set! result
          (cond
            ((not macro)
              (error "macro not found" exp))
            (compiled-macro?
              ((Cyc-get-cvar (cadr macro))
                exp
                (Cyc-er-rename use-env mac-env)
                (Cyc-er-compare? use-env)))
            (else
              (eval
                (list
                  (Cyc-get-cvar (cadr macro))
                  (list 'quote exp)
                  (Cyc-er-rename use-env mac-env)
                  (Cyc-er-compare? use-env))
                mac-env))))
;        (newline)
;        (display "/* ")
;        (display (list 'macro:expand exp macro compiled-macro?))
;        (newline)
;        (display (list result))
;        (display "*/ ")
          (macro:add-renamed-vars! use-env)
          result))

    (define (macro:add-renamed-vars! env)
      ;; TODO: change this to use a hash table
      (set! *macro:renamed-variables*
        (env:extend-environment
          (env:all-variables env)
          (env:all-values env)
          *macro:renamed-variables*)))

    #;(define (macro:cleanup expr)
      (define (clean expr bv) ;; Bound variables
         (cond 
           ((const? expr)      expr)
           ((prim? expr)       expr)
           ((quote? expr)      expr)
           ((define-c? expr)   expr)
           ((ref? expr)        
            ;; TODO: if symbol has been renamed and is not a bound variable,
            ;;       undo the rename
            expr)
           ((if? expr)
            `(if ,(clean (if->condition expr) bv)
                 ,(clean (if->then expr) bv)
                 ,(if (if-else? expr)
                      (clean (if->else expr) bv)
                      #f)))
           ((lambda? expr)
            `(lambda ,(lambda->formals expr)
               ,@(map (lambda (e) 
                        (clean e (cons (lambda-formals->list expr) 
                                       bv)))
                      (lambda->exp))))
           ;; At this point defines cannot be in lambda form. 
           ;; EG: (define (f x) ...)
           ((define? expr)
            (let ((bv* (cons (define->var expr) bv)))
              `(define ,(define->var expr)
                       ,@(map
                          (lambda (e) (clean e bv*)) 
                          (define->exp expr)))))
           ;; For now, assume set is not introducing a new binding
           ((set!? expr)
            `(set! ,(clean (set!->var expr) bv)
                   ,(clean (set!->exp expr) bv)))
           ((app? expr) 
            (map (lambda (e) (clean e bv)) 
                 expr))
           (else
            (error "macro cleanup unexpected expression: " expr)))
      (clean expr '())))
      
    ; TODO: get macro name, transformer
    ; TODO: let-syntax forms
  ))
