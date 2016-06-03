(import (scheme base)
        (scheme write)
        (scheme cyclone transforms)
        (scheme cyclone pretty-print)
        )

;; Original code:
;;(let ((x 1) (y 2) (z 3))
;;  (write
;;    (cons
;;      x
;;      (cons y z))))

(define code
'(((lambda ()
    0
    ((lambda (x$3 y$2 z$1)
       (write (cons x$3 (cons y$2 z$1))))
     1
     2
     3))))
)

;; thought - can either CPS or CPS-opti convert the CPS code
;; to prevent wrapping non-cont prims in functions, but just
;; add them directly to calling lambda's?
(pretty-print
  (cps-convert code))
