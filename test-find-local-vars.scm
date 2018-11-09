(import (scheme base) (scheme write) (scheme cyclone ast) (scheme cyclone util) (scheme cyclone pretty-print))

(define (find-local-vars sexp)
  (define (scan exp)
    (cond
     ((ast:lambda? exp)
      (for-each
        scan
        (ast:lambda-body exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) exp)
     ((define? exp) 
      (for-each
        scan
        (define->exp exp)))
     ((set!? exp)
      (for-each
        scan
        (set!->exp exp)))
     ((if? exp)       
      (scan (if->condition exp))
      (scan (if->then exp))
      (scan (if->else exp)))
     ((app? exp)
      (cond
        ((ast:lambda? (car exp))
;; TODO: want to find this:
;;                  ((lambda
;;                     (k$1080)
;;                     (if (Cyc-fast-eq
;;                           (car first$89$683)
;;                           (car row$90$684))
;;                       (k$1080 if-equal$76$674)
;;                       (k$1080 if-different$77$675)))
;;                   (lambda
;;                     (r$1079)
;;                     (Cyc-seq
;;                       (vector-set!
;;                         vec$79$677
;;                         i$88$682
;;                         r$1079)
;;                       ((cell-get lp$80$87$681)
;;                        k$1073
;;                        (Cyc-fast-plus i$88$682 1)
;;                        (cdr first$89$683)
;;                        (cdr row$90$684))))))))
         'TODO)
        (else
          (map scan exp))))
     (else 'todo)
  ))
  (scan sexp))

(define sexp
             '(lambda
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

;(pretty-print
;  (ast:ast->pp-sexp
;    (ast:sexp->ast sexp)))

(find-local-vars (ast:sexp->ast sexp))
