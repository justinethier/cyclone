(define-library (scheme cyclone macros)
  (import (scheme base)
          ;(scheme write) ;; Debug only
          (scheme eval) ;; TODO: without this line, compilation just
                        ;; silently fails. WTF??
          (scheme cyclone util))
  ; TODO: really need export-all for these cyclone libs!!
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

    (define (macro:load-env! defined-macros)
      (set! *macro:env* (env:extend-environment
                          (map car defined-macros)
                          (map cdr defined-macros)
                          env:the-empty-environment)))

    (define (macro:get-env) *macro:env*)

    (define (macro:get-defined-macros) *macro:defined-macros*)

    ;; Macro section
    (define (define-syntax? exp)
      (tagged-list? 'define-syntax exp))

    (define (macro:macro? exp defined-macros) (assoc (car exp) defined-macros))
    (define (macro:expand exp defined-macros)
      (let* ((macro (assoc (car exp) defined-macros))
             (compiled-macro? (or (macro? (Cyc-get-cvar (cdr macro)))
                                  (procedure? (cdr macro)))))
          ;; Invoke ER macro
          (cond
            ((not macro)
              (error "macro not found" exp))
            (compiled-macro?
              ((Cyc-get-cvar (cdr macro))
                exp
                (Cyc-er-rename 'todo-mac-env)
                Cyc-er-compare?))
            (else
              ;; Assume evaluated macro
              (let* ((env-vars (map car defined-macros))
                     (env-vals (map (lambda (v)
                                      (list 'macro (cdr v)))
                                    defined-macros))
                     ;; Pass defined macros so nested macros can be expanded
                     (env (create-environment env-vars env-vals)))
                (eval
                  (list
                    (cdr macro)
                    (list 'quote exp)
                    (Cyc-er-rename 'todo-mac-env)
                    Cyc-er-compare?)
                  env))))))

    ; TODO: get macro name, transformer
    ; TODO: let-syntax forms
  ))
