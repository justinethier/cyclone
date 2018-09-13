(import
  (scheme base)
  (scheme cyclone ast)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  (scheme write)
  (srfi 2)
)

(define (analyze:find-known-lambdas exp)
TODO: scan for well-known lambdas:
- app of a lambda is well-known, that's easy
- lambda can be passed as a cont. If we can identify all the places the cont is called (?) and it is not used for anything but calls, then I suppose that also qualifies as well-known.
  this is more problematic to generate code for, though.
  may need a lookup table of symbol to well-known function (if any)
- ?? must be other cases

  (define (scan exp)
    (cond
     ((ast:lambda? exp)
      (for-each
        (lambda (e)
          (scan e def-sym))
        (ast:lambda-body exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      exp)
     ((define? exp) #f) ;; TODO ??
     ((set!? exp) #f) ;; TODO ??
     ((if? exp)       
      (scan (if->condition exp) def-sym)
      (scan (if->then exp) def-sym)
      (scan (if->else exp) def-sym))
     ((app? exp)
     )
     (else #f)))

;(trace:error `(update-lambda-atv! ,syms ,value))
  (scan exp))

;; TEST code:
(define sexp '(
 (define my-iota
   (lambda-11-cont
     (k$361 n$1$243)
     ((lambda-9
        (n$5$244 list$6$245)
        ((lambda-8
           (lp$2$7$246)
           ((lambda-389
              (lp$2$7$246)
              ((lambda-1
                 (r$363)
                 ((cell-get lp$2$7$246) k$361 n$5$244 list$6$245))
               (set-cell!
                 lp$2$7$246
                 (lambda-7-cont
                   (k$365 n$8$247 list$9$248)
                   (if (zero?__inline__ n$8$247)
                     (k$365 list$9$248)
                     ((cell-get lp$2$7$246)
                      k$365
                      (Cyc-fast-sub n$8$247 1)
                      (cons (Cyc-fast-sub n$8$247 1) list$9$248)))))))
            (cell lp$2$7$246)))
         #f))
      n$1$243
      '())))
 (define *size* 511)
 (define classmax 3)
 (define typemax 12)
 (define *iii* 0)
 (define *kount* 0)
 (define *d* 8)
 (define *piececount* #f)
 (define *class* #f)
 (define *piecemax* #f)
 (define *puzzle* #f)
 (define *p* #f)
 (define fit
   (lambda-27-cont
     (k$394 i$10$249 j$11$250)
     ((lambda-25
        (end$12$251)
        ((lambda-23
           (lp$13$17$253)
           ((lambda-390
              (lp$13$17$253)
              ((lambda-12
                 (r$396)
                 ((cell-get lp$13$17$253) k$394 0))
               (set-cell!
                 lp$13$17$253
                 (lambda-22-cont
                   (k$398 k$18$254)
                   ((lambda-19-cont
                      (k$402)
                      (if (Cyc-fast-gt k$18$254 end$12$251)
                        (k$402 (Cyc-fast-gt k$18$254 end$12$251))
                        (if (vector-ref (vector-ref *p* i$10$249) k$18$254)
                          (k$402 (vector-ref
                                   *puzzle*
                                   (Cyc-fast-plus j$11$250 k$18$254)))
                          (k$402 #f))))
                    (lambda-15
                      (r$399)
                      (if r$399
                        (k$398 (Cyc-fast-gt k$18$254 end$12$251))
                        ((cell-get lp$13$17$253)
                         k$398
                         (Cyc-fast-plus k$18$254 1)))))))))
            (cell lp$13$17$253)))
         #f))
      (vector-ref *piecemax* i$10$249))))
))

(analyze:find-known-lambdas
  (ast:sexp->ast sexp))
