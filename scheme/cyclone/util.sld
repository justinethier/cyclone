(define-library (scheme cyclone util)
  (import (scheme base)
          (scheme char))
  ; TODO: really need export-all for these cyclone libs!!
  (export
    ;; Code analysis
    tagged-list?
    if?
    begin?
    lambda?
    ;; Code generation
    mangle
    mangle-global
    ;; Scheme library functions
    delete
    delete-duplicates
    list-insert-at!
    any
    every
    filter)
  (include "util.scm")
  (begin
    ;; Simplified versions of every/any from SRFI-1
    (define (any pred lst)
      (let any* ((l (map pred lst)))
          (cond
            ((null? l) #f) ; Empty list
            ((car l)   #t) ; Done
            (else 
               (any* (cdr l))))))
    (define (every pred lst)
      (let every* ((l (map pred lst)))
          (cond
            ((null? l) #t) ; Empty list
            ((car l)
               (every* (cdr l)))
            (else 
               #f))))
    ))
