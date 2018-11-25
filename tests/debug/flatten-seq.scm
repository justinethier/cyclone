(import (scheme base) (scheme write) (scheme cyclone util) (scheme cyclone pretty-print))

(define sexp
  '(Cyc-seq
         (set! b '(#f . #f))
         (Cyc-seq
           (set-car!  a 1)
           (Cyc-seq
             (set-cdr!  a '(2))
             ((fnc a1 a2 a3))))))

;; Flatten a list containing subcalls of a given symbol.
;; For example, the expression: 
;;
;;  '(Cyc-seq
;;         (set! b '(#f . #f))
;;         (Cyc-seq
;;           (set-car!  a 1)
;;           (Cyc-seq
;;             (set-cdr!  a '(2))
;;             ((fnc a1 a2 a3)))))
;;
;; becomes:
;;
;;  '(Cyc-seq
;;     (set! b '(#f . #f))
;;     (set-car! a 1)
;;     (set-cdr! a '(2))
;;     ((fnc a1 a2 a3)))
;;
(define (flatten-subcalls sexp sym)
  (define (flat sexp acc)
    (cond
      ((not (pair? sexp))
       acc)
      ((and (tagged-list? sym (car sexp)))
        (flat (cdar sexp) acc))
      (else ;;(pair? sexp)
        (flat (cdr sexp) (cons (car sexp) acc))))
  )
  (reverse
    (flat sexp '())))

(pretty-print (flatten-subcalls sexp 'Cyc-seq))
(pretty-print (flatten-subcalls '(a b c d e (f (g))) 'Cyc-seq))
