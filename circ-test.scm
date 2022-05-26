;; TODO: Temporary test file
(import (scheme base) (scheme write))
(define v1 (vector #f))
(define v2 (vector v1))
(vector-set! v1 0 v2)
(display v1)
(display (equal? v1 v2))
(newline)

(define v1 (vector 1 2 3))
(define v2 (vector 1 v1 3))
(vector-set! v1 1 v2)
(display v1)
;(display (equal? v1 v2))
(newline)

(define l1 (list #f))
(define l2 (list l1))
(set-cdr! l1 l2)
(display l1)
(display (equal? l1 l2))
(newline)

(define l1 (list 1 2 3))
(define l2 (list 1 l1 3))
(set-cdr! (cdr l1) l2)
(write l1)
(display (equal? l1 l2))
(newline)

; TODO: need to compare pointers to prevent this sort of thing:
;
; cyclone> (display #(1 1 1 1 1 1 1 1))
; #(... ... ... ... ... ... ... ...)
;
; for equalp of pairs we track progress using cdr, cddr
; proves that if pointers are equal we are traversing the same list
;
; how to handle vector traversal?
