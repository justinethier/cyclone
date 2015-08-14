(define-library (scheme cyclone macros)
  (import (scheme base)
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
      (let ((macro (assoc (car exp) defined-macros)))
        ;; assumes ER macro
        (if macro
          ((cdr macro) 
            ;exp 
            ; could be a raw lambda, if that is the case try quoting it
            (if (procedure? (cdr macro))
              exp
              (list 'quote exp))
            (lambda (sym) ;; TODO: not good enough, need to actually rename, and keep same results if
              sym)        ;; the same symbol is renamed more than once
            (lambda (sym-a sym-b) ;; TODO: the compare function from exrename.
              (eq? sym-a sym-b))) ;; this may need to be more sophisticated
          exp))) ;; TODO: error instead??

    ; TODO: get macro name, transformer
    ; TODO: base off of syntactic closures instead of ER macros??
    ; TODO: let-syntax forms
  ))
