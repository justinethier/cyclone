;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2018, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This file is part of the cps-optimizations module.
;;;;

(cond-expand 
  (program
    (import (scheme base) 
            (scheme write) 
            (scheme cyclone ast) 
            (scheme cyclone util) 
            (scheme cyclone pretty-print))))

;; Local variable reduction:
;; Reduce given sexp by replacing certain lambda calls with a let containing
;; local variables. Based on the way cyclone transforms code, this will
;; typically be limited to if expressions embedded in other expressions.
(define (opt:local-var-reduction sexp)
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
         (let ((value (lvr:tail-calls->values
                        (car (ast:lambda-body (car exp)))
                        (car (ast:lambda-args (car exp)))
                        (car (ast:lambda-args (cadr exp)))
                      ))
               (var (car (ast:lambda-args (cadr exp))))
               (body (ast:lambda-body (cadr exp))))
          `(let ((,var ,value))
            ,@body)))
        (else
          (map scan exp))))
     (else (error "unknown expression type: " exp))
  ))
  (scan sexp))

;; Local variable reduction helper:
;; Scan sexp to determine if sym is only called in a tail-call position
(define (lvr:local-tail-call-only? sexp sym)
  (call/cc
    (lambda (return)
      (define (scan exp fail?)
        ;;(write `(DEBUG lvr:local-tail-call-only? scan ,exp)) (newline)
        (cond
         ((ast:lambda? exp)
          (return #f)) ;; Could be OK if not ref'd...
         ((quote? exp) exp)
         ((const? exp) exp)
         ((ref? exp) 
          (if (equal? exp sym)
              (return #f))) ;; Assume not a tail call
         ((define? exp) 
          (return #f)) ;; Fail fast
         ((set!? exp)
          (return #f)) ;; Fail fast
         ((if? exp)       
          (scan (if->condition exp) #t) ;; fail if found under here
          (scan (if->then exp) fail?)
          (scan (if->else exp) fail?))
         ((app? exp)
          (cond
            ((and (equal? (car exp) sym)
                  (not fail?))
             (map (lambda (e) (scan e fail?)) (cdr exp))) ;; Sym is OK, skip
            (else
             (map (lambda (e) (scan e fail?)) exp))))
         (else exp)))
      (scan sexp #f)
      (return #t))))

;; Local variable reduction helper:
;; Transform all tail calls of sym in the sexp to just the value passed
(define (lvr:tail-calls->values sexp sym assign-sym)
  (call/cc
    (lambda (return)
      (define (scan exp)
        ;;(write `(DEBUG scan ,exp)) (newline)
        (cond
         ((ast:lambda? exp)
          (return #f)) ;; Could be OK if not ref'd...
         ((quote? exp) exp)
         ((const? exp) exp)
         ((ref? exp) 
          (if (equal? exp sym)
              (return #f))) ;; Assume not a tail call
         ((define? exp) 
          (return #f)) ;; Fail fast
         ((set!? exp)
          (return #f)) ;; Fail fast
         ((if? exp)       
          `(if ,(if->condition exp) 
               ,(scan (if->then exp))
               ,(scan (if->else exp))))
         ((app? exp)
          (cond
            ((and (equal? (car exp) sym)
                  (= (length exp) 2)
             )
             `(Cyc-local-set! ,assign-sym ,(cadr exp)))
            (else
             (return #f))))
         (else exp)))
      (return
        (scan sexp)))))

(cond-expand
  (program
    (define sexp
'(
 (define zunda
   ((lambda
      (k$1057 first-row-perm$61$668 mat$62$669)
      (first-row-perm$61$668
        (lambda
          (first-row$65$670)
          ((lambda
             (number-of-cols$68$671)
             ((lambda
                (make-row->func$71$672)
                (first-row-perm$61$668
                  (lambda
                    (r$1062)
                    (make-row->func$71$672
                      (lambda
                        (r$1063)
                        (make-row->func$71$672
                          (lambda
                            (r$1064)
                            (zebra k$1057
                                   r$1062
                                   r$1063
                                   r$1064
                                   (cdr mat$62$669)
                                   number-of-cols$68$671))
                          -1
                          1))
                      1
                      -1))
                  'child))
              (lambda
                (k$1066 if-equal$76$674 if-different$77$675)
                (k$1066
                  (lambda
                    (k$1067 row$78$676)
                    ((lambda
                       (vec$79$677)
                       ((lambda
                          (first$85$679 row$86$680)
                          ((lambda
                             (lp$80$87$681)
                             ((lambda
                                (lp$80$87$681)
                                (Cyc-seq
                                  (set!
                                    lp$80$87$681
                                    (lambda
                                      (k$1073 i$88$682 first$89$683 row$90$684)
                                      (if (Cyc-fast-eq
                                            i$88$682
                                            number-of-cols$68$671)
                                        (k$1073
                                          (Cyc-fast-eq
                                            i$88$682
                                            number-of-cols$68$671))
                                        ((lambda
                                           (k$1080)
                                           (if (Cyc-fast-eq

                                                 (car first$89$683)
                                                 (car row$90$684))
                                             (k$1080 if-equal$76$674)
                                             (k$1080 if-different$77$675)))
                                         (lambda
                                           (r$1079)
                                           (Cyc-seq
                                             (vector-set!
                                               vec$79$677
                                               i$88$682
                                               r$1079)
                                             ((cell-get lp$80$87$681)
                                              k$1073
                                              (Cyc-fast-plus i$88$682 1)
                                              (cdr first$89$683)
                                              (cdr row$90$684))))))))
                                  ((cell-get lp$80$87$681)
                                   (lambda
                                     (r$1069)
                                     (k$1067
                                       (lambda
                                         (k$1070 i$92$686)
                                         (k$1070
                                           (vector-ref vec$79$677 i$92$686)))))
                                   0
                                   first$85$679
                                   row$86$680)))
                              (cell lp$80$87$681)))
                           #f))
                        first-row$65$670
                        row$78$676))
                     (make-vector number-of-cols$68$671)))))))
           (length first-row$65$670)))
        'now))))
  (define *num-passed* 0) 
 (define write-to-string
   (lambda
     (k$3086 x$892$1775)
     (call-with-output-string
       k$3086
       (lambda
         (k$3088 out$893$1776)
         ((lambda
            (x$895$1777)
            ((lambda
               (wr$896$1778)
               (Cyc-seq
                 (set! wr$896$1778
                   (lambda
                     (k$3091 x$897$1779)
                     (if (pair? x$897$1779)
                       ((lambda
                          (k$3112)
                          (if (symbol? (car x$897$1779))
                            (if (pair? (cdr x$897$1779))
                              (if (null? (cddr x$897$1779))
                                (k$3112
                                  (assq (car x$897$1779)
                                        '((quote . "'")
                                          (quasiquote . "`")
                                          (unquote . ",")
                                          (unquote-splicing . ",@"))))
                                (k$3112 #f))
                              (k$3112 #f))
                            (k$3112 #f)))
                        (lambda
                          (tmp$900$902$1780)
                          (if tmp$900$902$1780
                            ((lambda
                               (s$903$1781)
                               (display
                                 (lambda
                                   (r$3094)
                                   (wr$896$1778 k$3091 (cadr x$897$1779)))
                                 (cdr s$903$1781)
                                 out$893$1776))
                             tmp$900$902$1780)
                            (display
                              (lambda
                                (r$3097)
                                (wr$896$1778
                                  (lambda
                                    (r$3098)
                                    ((lambda
                                       (lp$907$1783)
                                       (Cyc-seq
                                         (set! lp$907$1783
                                           (lambda
                                             (k$3103 ls$908$1784)
                                             (if (pair? ls$908$1784)
                                               (display
                                                 (lambda
                                                   (r$3105)
                                                   (wr$896$1778
                                                     (lambda
                                                       (r$3106)
                                                       (lp$907$1783
                                                         k$3103
                                                         (cdr ls$908$1784)))
                                                     (car ls$908$1784)))
                                                 " "
                                                 out$893$1776)
                                               (if (null? ls$908$1784)
                                                 (k$3103 #f)
                                                 (display
                                                   (lambda
                                                     (r$3110)
                                                     (write k$3103
                                                            ls$908$1784
                                                            out$893$1776))
                                                   " . "
                                                   out$893$1776)))))
                                         (lp$907$1783
                                           (lambda
                                             (r$3099)
                                             (display k$3091 ")" out$893$1776))
                                           (cdr x$897$1779))))
                                     #f))
                                  (car x$897$1779)))
                              "("
                              out$893$1776))))
                       (write k$3091 x$897$1779 out$893$1776))))
                 (wr$896$1778 k$3088 x$895$1777)))
             #f))
          x$892$1775)))))
 )

)

    ;(pretty-print
    ;  (ast:ast->pp-sexp
    ;    (ast:sexp->ast sexp)))
    
    (pretty-print
      (ast:ast->pp-sexp
        (opt:local-var-reduction (ast:sexp->ast sexp)))
    )
    ))
