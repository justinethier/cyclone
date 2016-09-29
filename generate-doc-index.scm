(import 
  (scheme base) 
  (scheme write)
  (scheme cyclone util)
  (srfi 1))

;; TODO: move this somewhere useful
(define (index-of lst x)
 (define (find lst idx)
   (cond 
     ((null? lst) #f)
     ((eq? (car lst) x) idx)
     (else (find (cdr lst) (+ idx 1)))))
 (find lst 0))

;; goal is:
;; [`load`](api/scheme/load.md#load)
;(define line "docs/api/scheme/base.md:- [`denominator`](#denominator)")
(define line "docs/api/scheme/process-context.md:- [`get-environment-variables`](#get-environment-variables)")
(let* ((lis (string->list line))
       (s-file 5)
       (e-file (index-of lis #\:))
       (file (substring line s-file e-file))
       (s-fnc (+ 1 (index-of lis #\`)))
       (e-fnc (+ s-fnc (index-of (string->list (substring line (+ 0 s-fnc) (string-length line))) #\`)))
       (fnc (substring line s-fnc e-fnc)))
 (display
  (string-append 
   "[`" fnc "`](" file "#" fnc ")")))
