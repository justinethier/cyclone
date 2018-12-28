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
            (scheme cyclone util) 
            (scheme cyclone pretty-print))))

;; TODO:
;; analyze call graph. not exactly sure how this is going to work yet, but the goal is to be able to figure out which
;; variables a primitive call is dependent upon. We then need to be able to query if any of those variables are mutated 
;; (ideally in fnc body) in which case we cannot inline the prim call.
;; 
#;(define (analyze:build-call-graph sexp)
  (define (scan exp)
    ;(write `(DEBUG scan ,exp)) (newline)
    (cond
     ((ast:lambda? exp)
      (ast:%make-lambda
        (ast:lambda-id exp)
        (ast:lambda-args exp)
        (map scan (ast:lambda-body exp))
        (ast:lambda-has-cont exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) exp)
     ((define? exp) 
      `(define
        ,(define->var exp)
        ,@(map scan (define->exp exp))))
     ((set!? exp)
      `(set!
         ,(scan (set!->var exp))
         ,(scan (set!->exp exp))))
     ((if? exp)       
      `(if ,(scan (if->condition exp))
           ,(scan (if->then exp))
           ,(scan (if->else exp))))
     ((app? exp)
      (cond
        ((and
          (list? exp)
          (ast:lambda? (car exp))
          (equal? (length exp) 2)
          (ast:lambda? (cadr exp))
          (list? (ast:lambda-args (cadr exp)))
          (equal? 1 (length (ast:lambda-args (cadr exp))))
          (lvr:local-tail-call-only? 
            (ast:lambda-body (car exp)) 
            (car (ast:lambda-args (car exp))))
          ;(tagged-list? 'Cyc-seq (car (ast:lambda-body (cadr exp)))) ;; TODO: DEBUG line, remove this once it works!
         )
         ;;(write `(tail-call-only? passed for ,exp)) (newline)
         ;;(write `(replace with ,(lvr:tail-calls->values 
         ;;                         (car (ast:lambda-body (car exp)))
         ;;                         (car (ast:lambda-args (car exp))))))
         ;;(newline)
;TODO: need to revisit this, may need to replace values with assignments to the "let" variable.
;would need to be able to carry that through to cgen and assign properly over there...
         (let* ((value (lvr:tail-calls->values
                         (car (ast:lambda-body (car exp)))
                         (car (ast:lambda-args (car exp)))
                         (car (ast:lambda-args (cadr exp)))
                       ))
                (var (car (ast:lambda-args (cadr exp))))
                (body (ast:lambda-body (cadr exp)))
                (av (cond-expand
                      (program #f)
                      (else (adb:get/default var #f))))
                (ref-count
                  (if av
                      (cond-expand
                        (program #f)
                        (else (adbv:ref-count av)))
                      1)) ;; Dummy value
                )
            (if (and (> ref-count 0)  ;; 0 ==> local var is never used
                     value)
                `(let ((,var ,value))
                  ,@body)
                (map scan exp)) ;; failsafe
        ))
        (else
          (map scan exp))))
     (else (error "unknown expression type: " exp))
  ))
  (scan sexp))


(cond-expand
  (program
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
    
    ;(pretty-print
    ;  (ast:ast->pp-sexp
    ;    (opt:local-var-reduction (ast:sexp->ast sexp)))
    ;)
))
