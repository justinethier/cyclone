(import (scheme base) (scheme write) (scheme cyclone util))

(define sexp
  '(Cyc-seq
         (set! b '(#f . #f))
         ((Cyc-seq
           (set-car!  a 1)
           ((Cyc-seq
             (set-cdr!  a '(2))))))))

;; TODO: goal is a single cyc-seq containing all expressions as a single list
