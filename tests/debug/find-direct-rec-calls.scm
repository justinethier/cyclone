(import
  (scheme base)
  (scheme cyclone ast)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  (scheme write)
  (srfi 2)
)

  (define (find-direct-recursive-calls exp)
    ;; Verify the continuation is simple and there is no closure allocation
    (define (check-cont k)
      (cond
        ((ref? k) #t)
        (else #f)))

    (define (check-args args)
      (define (check exp)
        (cond
          ((quote? exp) #t)
          ((const? exp) #t)
          ((ref? exp) #t)
          ((app? exp)
           (and 
             ;; Very conservative right now
             (member (car exp) '(car cdr))
             (check-args (cdr exp))))
          (else #f)))
      (every check args))

    (define (scan exp def-sym)
      (write `(scan ,def-sym ,exp)) (newline)
      (cond
       ((ast:lambda? exp)
        ;; Reject if there are nested functions
        #f)
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) 
        exp)
       ((define? exp) #f)
       ((set!? exp) #f)
       ((if? exp)       
        ;;(scan (if->condition exp) def-sym) ;; Not a tail call
        (scan (if->then exp) def-sym)
        (scan (if->else exp) def-sym))
       ((app? exp)
        (when (equal? (car exp) def-sym)
          (if (and
                (check-args (cddr exp))
                (check-cont (cadr exp)))
            (write `(direct recursive call ,exp))
            (write `(not a direct recursive call ,exp))
         )
        )
       )
       (else #f)))
    (for-each
      (lambda (exp)
        (write exp) (newline)
        (and-let* (((define? exp))
                    (def-exps (define->exp exp))
                   ((vector? (car def-exps)))
                   ((ast:lambda? (car def-exps)))
                   )
         (scan (car (ast:lambda-body (car def-exps))) (define->var exp))
         ))
        exp)
  )

;; TEST code:
(define sexp '(
 (define l18 #f)
 (define l12 #f)
 (define l6 #f)
 (define mas
   (lambda
     (k$247 x$4$135 y$3$134 z$2$133)
     (shorterp
       (lambda
         (r$248)
         (if r$248
           (mas (lambda
                  (r$249)
                  (mas (lambda
                         (r$250)
                         (mas (lambda
                                (r$251)
                                (mas k$247 r$249 r$250 r$251))
                              (cdr z$2$133)
                              x$4$135
                              y$3$134))
                       (cdr y$3$134)
                       z$2$133
                       x$4$135))
                (cdr x$4$135)
                y$3$134
                z$2$133)
           (k$247 z$2$133)))
       y$3$134
       x$4$135)))
 (define foldr
   (lambda
     (k$6632 func$362$3486 end$361$3485 lst$360$3484)
     (if (null? lst$360$3484)
       (k$6632 end$361$3485)
       (foldr (lambda
                (r$6635)
                (func$362$3486 k$6632 (car lst$360$3484) r$6635))
              func$362$3486
              end$361$3485
              (cdr lst$360$3484)))))
 (define _list-index
   (lambda
     (k$3902 e$2456$2587 lst$2455$2586)
     (if (null? lst$2455$2586)
       (k$3902 -1)
       (if (eq? (car lst$2455$2586) e$2456$2587)
         (k$3902 0)
         (_list-index
           (lambda
             (r$3908)
             (if (Cyc-fast-eq r$3908 -1)
               (k$3902 -1)
               (_list-index
                 (lambda
                   (r$3906)
                   (k$3902 (Cyc-fast-plus 1 r$3906)))
                 e$2456$2587
                 (cdr lst$2455$2586))))
           e$2456$2587
           (cdr lst$2455$2586))))))
 (define *num-passed* 1)
 (define shorterp
   (lambda
     (k$240 x$6$131 y$5$130)
     (if (null? y$5$130)
       (k$240 #f)
       (if (null? x$6$131)
         (k$240 (null? x$6$131))
         (shorterp k$240 (cdr x$6$131) (cdr y$5$130))))))
))

(find-direct-recursive-calls
  (ast:sexp->ast sexp))
