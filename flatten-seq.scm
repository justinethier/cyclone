(import (scheme base) (scheme write) (scheme cyclone util) (scheme cyclone pretty-print))

(define sexp
  '(Cyc-seq
         (set! b '(#f . #f))
         ((Cyc-seq
           (set-car!  a 1)
           ((Cyc-seq
             (set-cdr!  a '(2))))))))

;; Flatten a list containing subcalls of a given symbol.
;; For example, the expression: 
;;
;;  '(Cyc-seq
;;         (set! b '(#f . #f))
;;         ((Cyc-seq
;;           (set-car!  a 1)
;;           ((Cyc-seq
;;             (set-cdr!  a '(2)))))))
;;
;; becomes:
;;
;;  '(Cyc-seq
;;     (set! b '(#f . #f))
;;     (set-car!  a 1)
;;     (set-cdr!  a '(2)))
;;
(define (flatten-subcalls sexp sym)
  (define (flat sexp acc)
    (cond
      ((not (pair? sexp))
       acc)
      ((and (app? (car sexp))
            (app? (caar sexp))
            (tagged-list? sym (caar sexp)))
       (flat (cdaar sexp) acc))
      (else ;;(pair? sexp)
        (flat (cdr sexp) (cons (car sexp) acc))))
  )
  (reverse
    (flat sexp '())))

(pretty-print (flatten-subcalls sexp 'Cyc-seq))
(pretty-print (flatten-subcalls '(a b c d e (f (g))) 'Cyc-seq))
