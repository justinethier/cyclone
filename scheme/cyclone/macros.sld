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
          ;(scheme write) ;; Debug only
          (scheme eval)
          (scheme cyclone util)
  )
  (export
    define-syntax?
    macro:macro?
    macro:expand
    macro:add!
    macro:load-env!
    macro:get-env
    macro:get-defined-macros
  )
  (begin
    ;; top-level macro environment
    (define *macro:env* '())

    ;; A list of all macros defined by the program/library being compiled
    (define *macro:defined-macros* '())

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
      (let* ((compiled-macro? (or (macro? (Cyc-get-cvar (cadr macro)))
                                  (procedure? (cadr macro)))))
        ;(newline)
        ;(display "/* ")
        ;(display (list 'macro:expand exp macro compiled-macro?))
        ;(display "*/ ")

          ;; Invoke ER macro
          (cond
            ((not macro)
              (error "macro not found" exp))
            (compiled-macro?
              ((Cyc-get-cvar (cadr macro))
                exp
                (Cyc-er-rename mac-env)
                Cyc-er-compare?))
            (else
              (eval
                (list
                  (Cyc-get-cvar (cadr macro))
                  (list 'quote exp)
                  (Cyc-er-rename mac-env)
                  Cyc-er-compare?)
                mac-env)))))

    ; TODO: get macro name, transformer
    ; TODO: let-syntax forms
  ))
