(import (scheme base) (scheme write))

;;; Fails when compiling compiler benchmark:
;(define (proper-length l)
;  (define (length l n)
;    (cond ((pair? l) (length (cdr l) (+ n 1))) ((null? l) n) (else #f)))
;  (length l 0))
;
;
;;; One possible idea, alpha conversion could maintain a local lexical environment, "locals"
;;; and use that to rename identifiers that clash with primitives.
;;; Might be able to use a list of hash tables (or alists) for this purpose.
;;; Just load up a new list when a new scope is encountered, and add any
;;; "define", "set!", or "lambda" variables to it. then if a ref is a
;;; local, we need to rename it
;;; TODO: can we use same environments as "eval" uses for this? makes
;;; sense not to reinvent things
;
;(write (proper-length '(a b c)))


;; Seems graphs is timing out, possibly due to too many mutations 
;; in its loops. see "set!" introduced by macro expansion below:
(define size 10)
(let loop ((i 1))
  (if (< i size)
    (begin (write i) (loop (+ i 1)))))

;; Current macro expansion
(newline)
 ((lambda (i)
    ((lambda (loop)
       (set! loop
         (lambda (i)
           (if (< i size)
             ((lambda () (write i) (loop (+ i 1))))
             #f)))
       (loop i))
     #f))
  1)

