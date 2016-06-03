(import (scheme base)
        (scheme write)
(cps-optimizations)
;        (scheme cyclone transforms)
        (scheme cyclone pretty-print)
        )

;; Original code:
;;(let ((x 1) (y 2) (z 3))
;;  (write
;;    (cons
;;      x
;;      (cons y z))))
;
;(define code
;'(((lambda ()
;    0
;    ((lambda (x$3 y$2 z$1)
;       (write (cons x$3 (cons y$2 z$1))))
;     1
;     2
;     3))))
;)
;
;;; thought - can either CPS or CPS-opti convert the CPS code
;;; to prevent wrapping non-cont prims in functions, but just
;;; add them directly to calling lambda's?
;(pretty-print
;  (cps-convert code))
;
;
;((lambda (result)
; <lambda-body>)
; (prim ...))
;
;can we convert that to (assuming prim does not require a continuation):
;
; <lambda-body>
;
; with `(prim ...)` replacing any occurences of `result`.
; 
;then, what would this look like? ->
;also, what does it mean for other phases after CPS? 
;at a minimum, it is going to require changes to the cgen phase because that
;makes some assumptions about there only being one prim per function, I believe
;
;;; Original:
;#;(#((record-marker)
;   #((record-marker) #f (id args body))
;   #(6
;     ()
;     ((#((record-marker)
;         #((record-marker) #f (id args body))
;         #(5
;           (r$2)
;           ((#((record-marker)
;               #((record-marker) #f (id args body))
;               #(4
;                 (x$3 y$2 z$1)
;                 ((#((record-marker)
;                     #((record-marker) #f (id args body))
;                     #(3
;                       (r$4)
;                       ((#((record-marker)
;                           #((record-marker) #f (id args body))
;                           #(2
;                             (r$3)
;                             ((write #((record-marker)
;                                       #((record-marker) #f (id args body))
;                                       #(1 (r$1) ((r$1 %halt))))
;                                     r$3))))
;                         (cons x$3 r$4)))))
;                   (cons y$2 z$1)))))
;             1
;             2
;             3))))
;       0)))))
;
;;; TODO: update
;(#((record-marker)
;   #((record-marker) #f (id args body))
;   #(6
;     ()
;     ((#((record-marker)
;         #((record-marker) #f (id args body))
;         #(5
;           (r$2)
;           ((#((record-marker)
;               #((record-marker) #f (id args body))
;               #(4
;                 (x$3 y$2 z$1)
;                 ((write #((record-marker)
;                           #((record-marker) #f (id args body))
;                           #(1 (r$1) ((r$1 %halt))))
;                         (cons x$3 (cons y$2 z$1))))))
;             1
;             2
;             3))))
;       0)))))

(define code
'(#((record-marker)
   #((record-marker) #f (id args body))
   #(6
     ()
     ((#((record-marker)
         #((record-marker) #f (id args body))
         #(5
           (r$2)
           ((#((record-marker)
               #((record-marker) #f (id args body))
               #(4
                 (x$3 y$2 z$1)
                 ((#((record-marker)
                     #((record-marker) #f (id args body))
                     #(3
                       (r$4)
                       ((#((record-marker)
                           #((record-marker) #f (id args body))
                           #(2
                             (r$3)
                             ((write #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(1 (r$1) ((r$1 %halt))))
                                     r$3))))
                         (cons x$3 r$4)))))
                   (cons y$2 z$1)))))
             1
             2
             3))))
       0)))))
)
(pretty-print
  (contract-prims code))
