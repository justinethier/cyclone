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

;(define code
;'(#((record-marker)
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
;)

(define code
'((define in-port:read-buf!
   #((record-marker)
     #((record-marker) #f (id args body))
     #(621
       (k$807 ptbl$260)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(619
             (result$261)
             ((in-port:set-buf!
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(618 (r$809) ((k$807 result$261))))
                ptbl$260
                #f))))
         (cadr ptbl$260)))))))
)

;(define code
;'((define and
;   #((record-marker)
;     #((record-marker) #f (id args body))
;     #(2835
;       (k$3825 expr$1082 rename$1081 compare$1080)
;       ((#((record-marker)
;           #((record-marker) #f (id args body))
;           #(2834
;             (r$3836)
;             ((if (null? r$3836)
;                (k$3825 #t)
;                (#((record-marker)
;                   #((record-marker) #f (id args body))
;                   #(2832
;                     (r$3835)
;                     ((if (null? r$3835)
;                        (k$3825 (cadr expr$1082))
;                        (#((record-marker)
;                           #((record-marker) #f (id args body))
;                           #(2829
;                             (r$3834)
;                             ((rename$1081
;                                #((record-marker)
;                                  #((record-marker) #f (id args body))
;                                  #(2828
;                                    (r$3828)
;                                    ((#((record-marker)
;                                        #((record-marker) #f (id args body))
;                                        #(2827
;                                          (r$3829)
;                                          ((#((record-marker)
;                                              #((record-marker)
;                                                #f
;                                                (id args body))
;                                              #(2826
;                                                (r$3833)
;                                                ((rename$1081
;                                                   #((record-marker)
;                                                     #((record-marker)
;                                                       #f
;                                                       (id args body))
;                                                     #(2825
;                                                       (r$3831)
;                                                       ((list k$3825
;                                                              r$3828
;                                                              r$3829
;                                                              (cons r$3831
;                                                                    (cddr expr$1082))
;                                                              #f))))
;                                                   r$3833))))
;                                            'and))))
;                                      (cadr expr$1082)))))
;                                r$3834))))
;                         'if)))))
;                 (cddr expr$1082))))))
;         (cdr expr$1082))))))
;))

(pretty-print
  (optimize-cps code))
  ;(contract-prims code))
(write "---------------- cps analysis db:")
(pretty-print (adb:get-db))
