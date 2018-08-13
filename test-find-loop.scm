;; Temporary test program, find loops in original CPS, before optimizations
(import (scheme base) (scheme write)
 (scheme cyclone cps-optimizations)
 (scheme cyclone ast))

(define sexp
 (ast:sexp->ast '(define fit
   (lambda
     (k$89 i$10$52 j$11$53)
     ((lambda
        (lp$13$17$56)
        ((lambda (r$91) (lp$13$17$56 k$89 0))
         (set! lp$13$17$56
           (lambda
             (k$93 k$18$57)
             ((lambda
                (k$98)
                (if (Cyc-fast-gt
                      k$18$57
                      (vector-ref *piecemax* i$10$52))
                  (k$98 (Cyc-fast-gt
                          k$18$57
                          (vector-ref *piecemax* i$10$52)))
                  (if (vector-ref (vector-ref *p* i$10$52) k$18$57)
                    (k$98 (vector-ref
                            *puzzle*
                            (Cyc-fast-plus j$11$53 k$18$57)))
                    (k$98 #f))))
              (lambda
                (r$94)
                (if r$94
                  (if (Cyc-fast-gt
                        k$18$57
                        (vector-ref *piecemax* i$10$52))
                    (k$93 #t)
                    (k$93 #f))
                  (lp$13$17$56 k$93 (Cyc-fast-plus k$18$57 1)))))))))
      #f)))))

(write (ast:ast->pp-sexp sexp))
