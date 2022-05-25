;; TODO: Temporary test file
(import (scheme base) (scheme write))
(define v1 (vector #f))
(define v2 (vector v1))
(vector-set! v1 0 v2)
(display v1)
;(equal? v1 v2)

(define v1 (vector 1 2 3))
(define v2 (vector 1 v1 3))
(vector-set! v1 1 v2)
(display v1)

(define l1 (list #f))
(define l2 (list l1))
(set-cdr! l1 l2)
(display l1)
