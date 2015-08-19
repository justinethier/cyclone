(define-library (scheme cyclone macros)
  (import (scheme base)
          ;(scheme write)
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
        ;(display "/* ")
        ;(newline)
        ;(display "entered macro:expand exp")
        ;(display " */")
      (let ((rename (lambda (sym) ;; TODO: not good enough, need to actually rename, and keep same results if
                      sym))       ;; the same symbol is renamed more than once
            (compare? (lambda (sym-a sym-b)  ;; TODO: the compare function from exrename.
                         (eq? sym-a sym-b))) ;; this may need to be more sophisticated
           )
        (let ((macro (assoc (car exp) defined-macros)))

;TODO: restructure this to use eval if the macro is not a proc.
;      then can try passing in an environment with create-environment.
;      once eval is extended to work with macros, this could allow it to
;      expand a macro contained within another
        ;(display "/* ")
        ;(newline)
        ;(display (list macro (car exp) 
        ;              (Cyc-get-cvar (cdr macro))
        ;              (macro? (Cyc-get-cvar (cdr macro)))))
        ;(display " */")

          ;; Invoke ER macro
          (if macro
            ((Cyc-get-cvar (cdr macro))
              ;; Pass expression differently depending upon if this is a 
              ;; compiled macro or a cons one that will be called via eval.
              ;;
              ;; If a raw lambda (IE, exec using eval), try quoting it
              (if (or (macro? (Cyc-get-cvar (cdr macro)))
                      (procedure? (cdr macro)))
                exp
                (list 'quote exp))
              rename
              compare?)
            exp)))) ;; TODO: error instead??

    ; TODO: get macro name, transformer
    ; TODO: base off of syntactic closures instead of ER macros??
    ; TODO: let-syntax forms
  ))
