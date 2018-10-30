(import (scheme base) (scheme write) (scheme cyclone util))

(define sexp
  '(Cyc-seq
         (set! b '(#f . #f))
         ((Cyc-seq
           (set-car!  a 1)
           ((Cyc-seq
             (set-cdr!  a '(2))))))))

;; TODO: goal is a single cyc-seq containing all expressions as a single list
(define (convert sexp)
  (define (flat sexp acc)
    (write `(flat ,sexp)) (newline)
    (cond
      ((null? sexp) acc)
      ((tagged-list? 'Cyc-seq sexp)
       (flat (cdr sexp) acc))
      ((and (app? sexp)
            (tagged-list? 'Cyc-seq (car sexp)))
       (flat (cdar sexp) acc))
      (else 
        (flat (cdr sexp) (cons sexp acc))))
  )
  (reverse
    (flat sexp '(Cyc-seq))))

(write (convert sexp))
