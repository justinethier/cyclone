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

;; TODO:
;; analyze call graph. not exactly sure how this is going to work yet, but the goal is to be able to figure out which
;; variables a primitive call is dependent upon. We then need to be able to query if any of those variables are mutated 
;; (ideally in fnc body) in which case we cannot inline the prim call.
;; 
;; Notes:
;; Should we pass a copy of the current call graph and then dump it off when a new variable is encountered? In which case, when do we reset the graph? Maybe we just build it up as an a-list as we go, so it resets itself automatically? Then each a-list can exist as part of analysis DB for the variable... would that work?

;; Is it OK to inline code replacing ref, based on call graph data from lookup table?
(define (inline-ok-from-call-graph? ref tbl)
  (and-let* ((vars (hash-table-ref/default tbl ref #f)))
    (call/cc
      (lambda (return)
        (for-each 
          (lambda (v)
            (and-let* ((adb-var (adb:get/default v #f)))
              (when (not (adbv:inlinable adb-var))
                ;(trace:error `(cannot inline ,ref))
                (return #f))
            )
          )
          (cdr vars)) ;; Skip ref itself
        (return #t)))))

(define (analyze:build-call-graph sexp)
  ;; Add new entry for each var as it is found...
  (define lookup-tbl (make-hash-table))

  ;; Pass over the sexp
  ;; exp - S-expression to scan
  ;; vars - alist of current set of variables
  (define (scan exp vars)
    ;(trace:error `(DEBUG scan ,(ast:ast->pp-sexp exp)))
    (cond
     ((ast:lambda? exp)
      (for-each
        (lambda (a)
          (scan a vars))
        (ast:lambda-formals->list exp))
      (for-each
        (lambda (e)
          (scan e vars))
        (ast:lambda-body exp))
     )
     ((quote? exp) #f)
     ((const? exp) #f)
     ((ref? exp) 
      (hash-table-set! lookup-tbl exp vars)
     )
     ((define? exp) 
      (scan (define->exp exp) '()))
     ((set!? exp)
      ;; TODO: probably need to keep track of var here
      (scan (set!->var exp) vars)
      (scan (set!->exp exp) vars))
     ((if? exp)       
      (scan (if->condition exp) vars)
      (scan (if->then exp) vars)
      (scan (if->else exp) vars))
     ((app? exp)
      (cond
       ((ast:lambda? (car exp))
        ;; Track deps on lambda var(s)
        (for-each
          (lambda (e)
            (scan e vars))
          (ast:lambda-formals->list (car exp)))

        ;; Scan body, with reset vars (??)
        (for-each
          (lambda (e)
            (scan e '()))
          (ast:lambda-body (car exp))))
       ((and (ref? (car exp))
             (list? exp) 
             (> (length exp) 1))
         (let* ((cont (cadr exp))
                ;; TODO: what if arg is not a ref? Is that possible after cps (probably, with inlining)?
                (args (filter ref? (cddr exp)))
                (vars* (append args vars))
              )
         (scan cont vars*)
         ;(for-each
         ; (lambda (e)
         ;   (scan e vars*))
         ; (cdr exp))
       ))
       (else
        (for-each
          (lambda (e)
            (scan e vars))
          exp))))
     (else (error "unknown expression type: " exp))
  ))
  (scan sexp '())
  lookup-tbl)


(cond-expand
  (program
    (define trace:error write)
    (define sexp
'(

 (define test
   (lambda
     (k$38 obj$5$11)
     (queue->list
       (lambda
         (r$42)
         ((lambda
            (r$39)
            ((lambda
               (m$6$12)
               (queue-put!
                 (lambda
                   (r$40)
                   (queue-put!
                     (lambda (r$41) (k$38 m$6$12))
                     object-queue
                     obj$5$11))
                 objects-dumped
                 obj$5$11))
             r$39))
          (length r$42)))
       objects-dumped)))

 ;; Doesn't really matter, but lets leave this for now
 (define slot-set!
   (lambda
     (k$7170
       name$2424$3603
       obj$2425$3604
       idx$2426$3605
       val$2427$3606)
     ((lambda
        (vec$2428$3607)
        ((lambda
           (r$7171)
           (k$7170
             (vector-set! r$7171 idx$2426$3605 val$2427$3606)))
         (vector-ref vec$2428$3607 2)))
      obj$2425$3604)))
 )

)

    (pretty-print
      (ast:ast->pp-sexp
        (ast:sexp->ast sexp)))
    
    (newline)
    (newline)

    (let ((ht (analyze:build-call-graph (ast:sexp->ast sexp))))
      (pretty-print (hash-table->alist ht))
      (newline)
;; TODO: store table and call these to test various vars:
(analyze:find-inlinable-vars (ast:sexp->ast sexp) '()) ;; Identify variables safe to inline
(pretty-print (inline-ok-from-call-graph? 'r$39 ht))
(newline)
(pretty-print (inline-ok-from-call-graph? 'zzz ht))
(newline)
    )

    ;(pretty-print
    ;  (ast:ast->pp-sexp
    ;    (opt:local-var-reduction (ast:sexp->ast sexp)))
    ;)
))
