;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2019, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This file is part of the cps-optimizations module.
;;;;

(cond-expand 
  (program
    (import (scheme base) 
            (scheme write) 
            (scheme cyclone ast) 
            (scheme cyclone primitives)
            (scheme cyclone transforms)
            (scheme cyclone cps-optimizations)
            (scheme cyclone util) 
            (scheme cyclone pretty-print)
            (srfi 2)
            (srfi 69)
            )
    ))


(define (analyze:memoize-pure-fncs sexp)
;;  ;; Add new entry for each var as it is found...
;;  (define lookup-tbl (make-hash-table))
;;
;;  ;; Pass over the sexp
;;  ;; exp - S-expression to scan
;;  ;; vars - alist of current set of variables
;;  (define (scan exp vars)
;;    ;(trace:error `(DEBUG scan ,(ast:ast->pp-sexp exp)))
;;    (cond
;;     ((ast:lambda? exp)
;;      (for-each
;;        (lambda (a)
;;          (scan a vars))
;;        (ast:lambda-formals->list exp))
;;      (for-each
;;        (lambda (e)
;;          (scan e vars))
;;        (ast:lambda-body exp))
;;     )
;;     ((quote? exp) #f)
;;     ((const? exp) #f)
;;     ((ref? exp) 
;;      (hash-table-set! lookup-tbl exp vars)
;;     )
;;     ((define? exp) 
;;      (scan (define->exp exp) '()))
;;     ((set!? exp)
;;      ;; TODO: probably need to keep track of var here
;;      (scan (set!->var exp) vars)
;;      (scan (set!->exp exp) vars))
;;     ((if? exp)       
;;      (scan (if->condition exp) vars)
;;      (scan (if->then exp) vars)
;;      (scan (if->else exp) vars))
;;     ((app? exp)
;;      (cond
;;       ((ast:lambda? (car exp))
;;        ;; Track deps on lambda var(s)
;;        (for-each
;;          (lambda (e)
;;            (scan e vars))
;;          (ast:lambda-formals->list (car exp)))
;;        ;; Scan body, with reset vars (??)
;;        (for-each
;;          (lambda (e)
;;            (scan e '()))
;;          (ast:lambda-body (car exp)))
;;        ;; Scan lambda arg(s), again also with reset vars
;;        (for-each
;;          (lambda (e)
;;            (scan e '()))
;;          (cdr exp))
;;       )
;;       ((and (ref? (car exp))
;;             (list? exp) 
;;             (> (length exp) 1))
;;         (let* ((cont (cadr exp))
;;                ;; TODO: what if arg is not a ref? Is that possible after cps (probably, with inlining)?
;;                (args (filter ref? (cddr exp)))
;;                (vars* (append args vars))
;;              )
;;         (scan cont vars*)
;;         ;(for-each
;;         ; (lambda (e)
;;         ;   (scan e vars*))
;;         ; (cdr exp))
;;       ))
;;       (else
;;        (for-each
;;          (lambda (e)
;;            (scan e vars))
;;          exp))))
;;     (else (error "unknown expression type: " exp))
;;  ))
;;  (scan sexp '())
;;  lookup-tbl
)


(cond-expand
  (program
    (define (trace:error exp)
      (write exp)
      (newline))
    (define sexp
'(

 (define fnc
   (lambda
     (k$41 x$5$21 y$6$22)
     (k$41 (Cyc-fast-plus x$5$21 y$6$22))))
 (define mfnc #f)
 (define ack
   (lambda
     (k$46 m$7$23 n$8$24)
     ((lambda
        (r$47)
        (if r$47
          ((lambda () (k$46 (Cyc-fast-plus n$8$24 1))))
          ((lambda
             (r$48)
             (if r$48
               ((lambda
                  ()
                  ((lambda (r$49) (ack k$46 r$49 1))
                   (Cyc-fast-sub m$7$23 1))))
               ((lambda
                  ()
                  ((lambda
                     (r$50)
                     ((lambda
                        (r$52)
                        (ack (lambda (r$51) (ack k$46 r$50 r$51))
                             m$7$23
                             r$52))
                      (Cyc-fast-sub n$8$24 1)))
                   (Cyc-fast-sub m$7$23 1))))))
           (Cyc-fast-eq n$8$24 0))))
      (Cyc-fast-eq m$7$23 0))))
 (define fib
   (lambda
     (k$55 n$16$25)
     ((lambda
        (r$56)
        (if r$56
          (k$55 n$16$25)
          ((lambda
             (r$60)
             (fib (lambda
                    (r$57)
                    ((lambda
                       (r$59)
                       (fib (lambda
                              (r$58)
                              (k$55 (Cyc-fast-plus r$57 r$58)))
                            r$59))
                     (Cyc-fast-sub n$16$25 2)))
                  r$60))
           (Cyc-fast-sub n$16$25 1))))
      (Cyc-fast-lt n$16$25 2))))

))

;;    (pretty-print
;;      (ast:ast->pp-sexp
;;        (ast:sexp->ast sexp)))
;;    
;;    (newline)
;;    (newline)
;;
;;    (let ((ht (analyze:build-call-graph (ast:sexp->ast sexp))))
;;      (pretty-print (hash-table->alist ht))
;;      (newline)
;;;; TODO: store table and call these to test various vars:
;;(analyze:find-inlinable-vars (ast:sexp->ast sexp) '()) ;; Identify variables safe to inline
;;(pretty-print (inline-ok-from-call-graph? 'm$30$47 ht))
;;(newline)
;;(pretty-print (inline-ok-from-call-graph? 'zzz ht))
;;(newline)
;;    )
;;
;;    ;(pretty-print
;;    ;  (ast:ast->pp-sexp
;;    ;    (opt:local-var-reduction (ast:sexp->ast sexp)))
;;    ;)
))
