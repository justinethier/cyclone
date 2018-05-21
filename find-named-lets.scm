(import
  (scheme base)
  (scheme cyclone ast)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  (scheme write)
  (srfi 2)
)

;; TODO: can we scan the ast to find loops created by named lets?
;;
;; This is the typical structure of such a loop:
;; 
;;              ((lambda (loop$14$171)
;;                 (set! loop$14$171
;;                   (lambda (zr$17$174 zi$16$173 c$15$172)
;;                     (...)))
;;                 (loop$14$171 zr$13$170 zi$12$169 c$11$168))
;;               #f)

    (define (scan exp)
      (cond
       ((ast:lambda? exp)
        (let* ((id (ast:lambda-id exp))
               (has-cont (ast:lambda-has-cont exp))
               (sym (string->symbol 
                      (string-append
                         "lambda-"
                         (number->string id)
                         (if has-cont "-cont" ""))))
              )
          `(,sym ,(ast:lambda-args exp)
             ,@(map scan (ast:lambda-body exp))))
       )
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) exp)
       ((define? exp)
        `(define ,(define->var exp)
                 ,@(scan (define->exp exp))))
       ((set!? exp)
        `(set! ,(set!->var exp)
               ,(scan (set!->exp exp))))
       ((if? exp)       
        `(if ,(scan (if->condition exp))
             ,(scan (if->then exp))
             ,(scan (if->else exp))))
       ((app? exp)
        (cond
          ((and-let* ( 
             ;; Find lambda with initial #f assignment
             ((lambda? (car exp)))
             ((pair? (cdr exp)))
             ((not (cadr exp)))
             (= 1 (length (lambda->formals (car exp))))
             ;; Get information for continuation
             (loop-sym (car (lambda->formals (car exp))))
             (inner-exp (car (lambda->exp (car exp))))
             ((app? inner-exp))
             ((lambda? (car inner-exp)))
             ;; Find the set (assumes CPS conversion)
             ((pair? (cdr inner-exp)))
             ((set!? (cadr inner-exp)))
             ((equal? (set!->var (cadr inner-exp)) loop-sym))
             ;; Check the set's continuation
             ((app? (car (lambda->exp (car inner-exp)))))
             ((equal? (caar (lambda->exp (car inner-exp))) loop-sym))
            )
           (write `(found named lambda loop ,loop-sym))
           ;; Continue scanning
           (map scan exp)
          ))
          (else
            (map scan exp))))
       (else exp)))

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

(scan 
  ;(ast:sexp->ast 
    sexp);)

