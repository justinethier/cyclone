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
'((define reg-port
   #((record-marker)
     #((record-marker) #f (id args body))
     #(630
       (k$812 fp$262)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(629
             (r$813)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(628
                   (r$263)
                   ((if r$263
                      (#((record-marker)
                         #((record-marker) #f (id args body))
                         #(622 () ((k$812 r$263)))))
                      (#((record-marker)
                         #((record-marker) #f (id args body))
                         #(627
                           ()
                           ((list #((record-marker)
                                    #((record-marker) #f (id args body))
                                    #(626
                                      (r$817)
                                      ((#((record-marker)
                                          #((record-marker) #f (id args body))
                                          #(625
                                            (r$814)
                                            ((#((record-marker)
                                                #((record-marker)
                                                  #f
                                                  (id args body))
                                                #(624
                                                  (r$816)
                                                  ((#((record-marker)
                                                      #((record-marker)
                                                        #f
                                                        (id args body))
                                                      #(623
                                                        (r$815)
                                                        ((k$812 r$263))))
                                                    (set! *in-port-table*
                                                      r$816)))))
                                              (cons r$263 *in-port-table*)))))
                                        (set! r$263 r$817)))))
                                  fp$262
                                  #f
                                  1
                                  0)))))))))
               r$813))))
         (assoc fp$262 *in-port-table*)))))))
)

(pretty-print
  (optimize-cps code))
  ;(contract-prims code))
(write "---------------- cps analysis db:")
(pretty-print (adb:get-db))
