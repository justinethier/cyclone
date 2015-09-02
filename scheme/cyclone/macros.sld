(define-library (scheme cyclone macros)
  (import (scheme base)
          (scheme write) ;; Debug only
          (scheme eval) ;; TODO: without this line, compilation just
                        ;; silently fails. WTF??
          (scheme cyclone util))
  ; TODO: really need export-all for these cyclone libs!!
  (export
    define-syntax?
    macro:macro?
    macro:expand
    macro:expand2
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
;TODO: seems to be a problem with below. may want to revert and try and see if that works, then
;restore this and debug...
;
; TODO: probably want to look at contents of defined-macros and figure out
; exactly how the env needs to represent macros (compiled and eval'd)
;
    (define (macro:expand _ exp mac-env defined-macros)
      (let* ((macro (assoc (car exp) defined-macros))
             (compiled-macro? (or (macro? (Cyc-get-cvar (cdr macro)))
                                  (procedure? (cdr macro)))))
        ;(newline)
        ;(display "/* ")
        ;(display (list 'macro:expand exp macro compiled-macro?))
        ;(display "*/ ")
          ;; Invoke ER macro
          (cond
            ((not macro)
              (error "macro not found" exp))
            (compiled-macro?
              ((Cyc-get-cvar (cdr macro))
                exp
                (Cyc-er-rename mac-env)
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
                    (Cyc-get-cvar (cdr macro))
                    (list 'quote exp)
                    (Cyc-er-rename mac-env)
                    Cyc-er-compare?)
                  env))))))
                  ;mac-env))))))

    (define (macro:expand2 exp macro mac-env)
      (let* ((compiled-macro? (or (macro? (Cyc-get-cvar (cadr macro)))
                                  (procedure? (cadr macro)))))
        ;(newline)
        ;(display "/* ")
        ;(display (list 'macro:expand2 exp macro compiled-macro?))
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
