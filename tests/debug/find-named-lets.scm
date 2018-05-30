(import
  (scheme base)
  (scheme cyclone ast)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  (scheme write)
  (srfi 2)
)

;; TODO:
;; - identify refs within named lets
;;   and also, whether refs are defined (or not) in loop
;; - will probably need to hook into analysis DB in production version
;; - will this find function work for optimized CPS? should test that, too
;; - does find need to be more robust? Are there false positives?

  (define (find-named-lets exp)
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
                (write `(,a defined in a loop))
                (newline))
              args)
          )
          `(,sym ,(ast:lambda-args exp)
             ,@(map (lambda (e) (scan e lp)) (ast:lambda-body exp))))
       )
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) 
        (when lp
            (write `(found variable ,exp within a loop))
            (newline))
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
           (write `(found named lambda loop ,loop-sym))
           ;; Continue scanning
           (map (lambda (e) (scan e #t)) exp)
          ))
          (else
            (map (lambda (e) (scan e lp)) exp))))
       (else exp)))
    (scan exp #f))

;; Test code follows:
(define sexp
 '(define count
     (lambda
       (k$296 r$5$163
              i$4$162
              step$3$161
              x$2$160
              y$1$159)
       ((lambda
          (loop$14$171)
          ((lambda
             (r$299)
             (loop$14$171
               k$296
               (Cyc-fast-plus
                 r$5$163
                 (Cyc-fast-mul
                   (inexact__inline__ x$2$160)
                   step$3$161))
               (Cyc-fast-plus
                 i$4$162
                 (Cyc-fast-mul
                   (inexact__inline__ y$1$159)
                   step$3$161))
               0))
           (set! loop$14$171
             (lambda
               (k$301 zr$17$174 zi$16$173 c$15$172)
               (if (Cyc-fast-eq c$15$172 64)
                 (k$301 c$15$172)
                 (if (Cyc-fast-gt
                       (Cyc-fast-plus
                         (Cyc-fast-mul zr$17$174 zr$17$174)
                         (Cyc-fast-mul zi$16$173 zi$16$173))
                       16.0)
                   (k$301 c$15$172)
                   (loop$14$171
                     k$301
                     (Cyc-fast-plus
                       (Cyc-fast-sub
                         (Cyc-fast-mul zr$17$174 zr$17$174)
                         (Cyc-fast-mul zi$16$173 zi$16$173))
                       (Cyc-fast-plus
                         r$5$163
                         (Cyc-fast-mul
                           (inexact__inline__ x$2$160)
                           step$3$161)))
                     (Cyc-fast-plus
                       (Cyc-fast-mul
                         2.0
                         (Cyc-fast-mul zr$17$174 zi$16$173))
                       (Cyc-fast-plus
                         i$4$162
                         (Cyc-fast-mul
                           (inexact__inline__ y$1$159)
                           step$3$161)))
                     (Cyc-fast-plus c$15$172 1))))))))
        #f))))

(define sexp-after-cps-no-opts
'((define count
   (lambda-165-cont
     (k$296 r$5$163
            i$4$162
            step$3$161
            x$2$160
            y$1$159)
     ((lambda-164
        (max-count$7$165 radius^2$6$164)
        ((lambda-163
           (r$316)
           ((lambda-162
              (r$315)
              ((lambda-161
                 (r$297)
                 ((lambda-160
                    (r$314)
                    ((lambda-159
                       (r$313)
                       ((lambda-158
                          (r$298)
                          ((lambda-157
                             (cr$9$167 ci$8$166)
                             ((lambda-156
                                (zr$13$170 zi$12$169 c$11$168)
                                ((lambda-155
                                   (loop$14$171)
                                   ((lambda-140
                                      (r$300)
                                      ((lambda-139
                                         (r$299)
                                         (loop$14$171
                                           k$296
                                           zr$13$170
                                           zi$12$169
                                           c$11$168))
                                       (set! loop$14$171 r$300)))
                                    (lambda-154-cont
                                      (k$301 zr$17$174 zi$16$173 c$15$172)
                                      ((lambda-153
                                         (r$302)
                                         (if r$302
                                           (k$301 c$15$172)
                                           ((lambda-152
                                              (r$303)
                                              ((lambda-151
                                                 (r$304)
                                                 ((lambda-150
                                                    (zr^2$19$176 zi^2$18$175)
                                                    ((lambda-149
                                                       (r$312)
                                                       ((lambda-148
                                                          (r$305)
                                                          (if r$305
                                                            (k$301 c$15$172)
                                                            ((lambda-147
                                                               (r$311)
                                                               ((lambda-146
                                                                  (r$306)
                                                                  ((lambda-145
                                                                     (r$310)
                                                                     ((lambda-144
                                                                        (r$309)
                                                                        ((lambda-143
                                                                           (r$307)
                                                                           ((lambda-142
                                                                              (new-zr$21$178
                                                                                new-zi$20$177)
                                                                              ((lambda-141
                                                                                 (r$308)
                                                                                 (loop$14$171
                                                                                   k$301
                                                                                   new-zr$21$178
                                                                                   new-zi$20$177
                                                                                   r$308))
                                                                               (Cyc-fast-plus
                                                                                 c$15$172
                                                                                 1)))
                                                                            r$306
                                                                            r$307))
                                                                         (Cyc-fast-plus
                                                                           r$309
                                                                           ci$8$166)))
                                                                      (Cyc-fast-mul
                                                                        2.0
                                                                        r$310)))
                                                                   (Cyc-fast-mul
                                                                     zr$17$174
                                                                     zi$16$173)))
                                                                (Cyc-fast-plus
                                                                  r$311
                                                                  cr$9$167)))
                                                             (Cyc-fast-sub
                                                               zr^2$19$176
                                                               zi^2$18$175))))
                                                        (Cyc-fast-gt
                                                          r$312
                                                          radius^2$6$164)))
                                                     (Cyc-fast-plus
                                                       zr^2$19$176
                                                       zi^2$18$175)))
                                                  r$303
                                                  r$304))
                                               (Cyc-fast-mul
                                                 zi$16$173
                                                 zi$16$173)))
                                            (Cyc-fast-mul
                                              zr$17$174
                                              zr$17$174))))
                                       (Cyc-fast-eq
                                         c$15$172
                                         max-count$7$165)))))
                                 #f))
                              cr$9$167
                              ci$8$166
                              0))
                           r$297
                           r$298))
                        (Cyc-fast-plus i$4$162 r$313)))
                     (Cyc-fast-mul r$314 step$3$161)))
                  (inexact__inline__ y$1$159)))
               (Cyc-fast-plus r$5$163 r$315)))
            (Cyc-fast-mul r$316 step$3$161)))
         (inexact__inline__ x$2$160)))
      64
      16.0)))))

(find-named-lets
  (ast:sexp->ast 
    sexp-after-cps-no-opts))
    ;;sexp))


