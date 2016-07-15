(import (scheme base) (scheme write))

;; Fails when compiling compiler benchmark:
(define (proper-length l)
  (define (length l n)
    (cond ((pair? l) (length (cdr l) (+ n 1))) ((null? l) n) (else #f)))
  (length l 0))


;; One possible idea, alpha conversion could maintain a local lexical environment, "locals"
;; and use that to rename identifiers that clash with primitives.
;; Might be able to use a list of hash tables (or alists) for this purpose.
;; Just load up a new list when a new scope is encountered, and add any
;; "define", "set!", or "lambda" variables to it. then if a ref is a
;; local, we need to rename it
;; TODO: can we use same environments as "eval" uses for this? makes
;; sense not to reinvent things

(write (proper-length '(a b c)))
