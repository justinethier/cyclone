;; Temporary test program, find loops in original CPS, before optimizations
(import (scheme base) (scheme write)
 (scheme cyclone cps-optimizations)
 (scheme cyclone util)
 (scheme cyclone ast))

(define (trace:error exp)
  (write exp)
  (newline))

(define (analyze:find-named-lets exp)
  (define (scan exp lp)
    (cond
     ((ast:lambda? exp)
      (let* ((id (ast:lambda-id exp))
             (has-cont (ast:lambda-has-cont exp))
             (sym (string->symbol 
                    (string-append
                       "lambda-"
                       (number->string id)
                       (if has-cont "-cont" ""))))
             (args* (ast:lambda-args exp))
             (args (if (null? args*) 
                       '() 
                       (formals->list args*)))
            )
        (when lp
          (for-each
            (lambda (a)
              (trace:error `(def in loop ,a))
              ;(with-var! a (lambda (var)
              ;  (adbv:set-def-in-loop! var #t)))
              )
            args))
        `(,sym ,(ast:lambda-args exp)
           ,@(map (lambda (e) (scan e lp)) (ast:lambda-body exp))))
     )
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      (when lp
        (trace:error `(found var ref ,exp in loop))
        ;(with-var! exp (lambda (var)
        ;  (adbv:set-ref-in-loop! var #t)))
        )
      exp)
     ((define? exp)
      `(define ,(define->var exp)
               ,@(scan (define->exp exp) lp)))
     ((set!? exp)
      `(set! ,(set!->var exp)
             ,(scan (set!->exp exp) lp)))
     ((if? exp)       
      `(if ,(scan (if->condition exp) lp)
           ,(scan (if->then exp) lp)
           ,(scan (if->else exp) lp)))
     ((app? exp)
      (cond
        ;; TODO: need to detect CPS pattern without any optimizations
        ;((and-let* (
        ;           )
        ; 'TODO
        ;)
        ((and-let* ( 
           ;; Find lambda with initial #f assignment
           ((ast:lambda? (car exp)))
           ((pair? (cdr exp)))
           ((not (cadr exp)))
           (= 1 (length (ast:lambda-args (car exp))))
           ;; Get information for continuation
           (loop-sym (car (ast:lambda-args (car exp))))
           (inner-exp (car (ast:lambda-body (car exp))))
           ((app? inner-exp))
           ((ast:lambda? (car inner-exp)))
           ;; Find the set (assumes CPS conversion)
           ((pair? (cdr inner-exp)))
           ((set!? (cadr inner-exp)))
           ((equal? (set!->var (cadr inner-exp)) loop-sym))
           ;; Check the set's continuation
           ((app? (car (ast:lambda-body (car inner-exp)))))
           ((equal? (caar (ast:lambda-body (car inner-exp))) loop-sym))
          )
         (trace:error `(found loop in ,exp))
         ;; TODO: do we want to record the lambda that is a loop?
         ;; Continue scanning, indicating we are in a loop
         (map (lambda (e) (scan e #t)) exp)
        ))
        (else
          (map (lambda (e) (scan e lp)) exp))))
     (else exp)))
  (scan exp #f))

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

;; Before any rounds of optimizations the structure is:
; (define fit
;   (lambda
;     (k$89 i$10$52 j$11$53)
;     ((lambda
;        (r$90)
;        ((lambda
;           (end$12$54)
;           ((lambda
;              (k$16$55)
;              ((lambda
;                 (lp$13$17$56)
;                 ((lambda
;                    (r$92)
;                    ((lambda (r$91) (lp$13$17$56 k$89 k$16$55))
;                     (set! lp$13$17$56 r$92)))
;                  (lambda
;                    (k$93 k$18$57)
;                    ((lambda
;                       (r$97)
;                       ((lambda
;                          (tmp$20$22$58)
;                          ((lambda
;                             (k$98)
;                             (if tmp$20$22$58
;                               (k$98 tmp$20$22$58)
;                               ((lambda
;                                  (r$101)
;                                  ((lambda
;                                     (r$99)
;                                     (if r$99
;                                       ((lambda
;                                          (r$100)
;                                          (k$98 (vector-ref *puzzle* r$100)))
;                                        (Cyc-fast-plus j$11$53 k$18$57))
;                                       (k$98 #f)))
;                                   (vector-ref r$101 k$18$57)))
;                                (vector-ref *p* i$10$52))))
;                           (lambda
;                             (r$94)
;                             (if r$94
;                               ((lambda (r$95) (if r$95 (k$93 #t) (k$93 #f)))
;                                (Cyc-fast-gt k$18$57 end$12$54))
;                               ((lambda (r$96) (lp$13$17$56 k$93 r$96))
;                                (Cyc-fast-plus k$18$57 1))))))
;                        r$97))
;                     (Cyc-fast-gt k$18$57 end$12$54)))))
;               #f))
;            0))
;         r$90))
;      (vector-ref *piecemax* i$10$52))))

(write (ast:sexp->ast sexp))
(newline)
(analyze:find-named-lets (ast:sexp->ast sexp))
