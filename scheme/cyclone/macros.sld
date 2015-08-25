(define-library (scheme cyclone macros)
  (import (scheme base)
          (scheme eval) ;; TODO: without this line, compilation just
                        ;; silently fails. WTF??
          (scheme cyclone util))
  ; TODO: really need export-all for these cyclone libs!!
  (export
    define-syntax?
    macro:macro?
    macro:expand
    macro:add!
    macro:get-defined-macros
  )
  (begin
    ;; A list of all macros defined by the program/library being compiled
    (define *macro:defined-macros* '())

    (define (macro:add! name body)
      (set! *macro:defined-macros* 
        (cons (cons name body) *macro:defined-macros*))
      #t)

    (define (macro:get-defined-macros) *macro:defined-macros*)

    ;; Macro section
    ;; TODO: place this in another module? could speed development
    (define (define-syntax? exp)
      (tagged-list? 'define-syntax exp))

    (define (macro:macro? exp defined-macros) (assoc (car exp) defined-macros))
    (define (macro:expand exp defined-macros)
      (let* (
             ;; TODO: not good enough, need to actually rename, 
             ;; and keep same results if
             ;; the same symbol is renamed more than once
             (rename (lambda (sym) 
                       sym))       
             ;; TODO: the compare function from exrename.
             ;; this may need to be more sophisticated
             (compare? (lambda (sym-a sym-b)  
                          (eq? sym-a sym-b))) 
             (macro (assoc (car exp) defined-macros))
             (compiled-macro? (or (macro? (Cyc-get-cvar (cdr macro)))
                                  (procedure? (cdr macro)))))

;TODO: restructure this to use eval if the macro is not a proc.
;      then can try passing in an environment with create-environment.
;      once eval is extended to work with macros, this could allow it to
;      expand a macro contained within another

          ;; Invoke ER macro
          (cond
            ((not macro)
              (error "macro not found" exp))
            (compiled-macro?
              ((Cyc-get-cvar (cdr macro))
                exp
                rename
                compare?))
            (else
              (let* ((env-vars (map car defined-macros))
                     (env-vals (map (lambda (v)
                                      (list 'macro (cdr v)))
                                    defined-macros))
                     (env (create-environment env-vars env-vals)))
                ;; Assume evaluated macro
                (eval
                  (list
                    (cdr macro)
                    (list 'quote exp)
                    rename
                    compare?)
                  ;env
                ))
              ))))

    ; TODO: get macro name, transformer
    ; TODO: base off of syntactic closures instead of ER macros??
    ; TODO: let-syntax forms
  ))
