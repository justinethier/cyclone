(define-library (scheme cyclone macros)
  (import (scheme base)
          (scheme cyclone util))
  ; TODO: really need export-all for these cyclone libs!!
  (export
    define-syntax?
  )
  (begin
    ;; Macro section
    ;; TODO: place this in another module? could speed development
    (define (define-syntax? exp)
      (tagged-list? 'define-syntax exp))

    ; TODO: get macro name, transformer
    ; TODO: base off of syntactic closures instead of ER macros??
    ; TODO: let-syntax forms
  ))
