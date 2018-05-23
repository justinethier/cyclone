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

(find-named-lets
  (ast:sexp->ast 
    sexp))

