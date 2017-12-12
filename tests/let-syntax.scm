(import (scheme base) (scheme write) (scheme cyclone pretty-print))

(let-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1)) (if temp temp (my-or e2 ...)))))))
  (let ((x #f) 
        (y 7) 
        (temp 8) 
        (my-let odd?) 
        (my-if even?))
    (my-or x (my-let temp) (my-if y) y))) ;; ==> 7

#;(letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1)) (if temp temp (my-or e2 ...)))))))
  (let ((x #f) 
        (y 7) 
        (temp 8) 
        (let odd?) 
        (if even?))
    (my-or x (let temp) (if y) y))) ;; ==> 7


;; From Chibi
#;(let ()
  (letrec-syntax ()
    (define internal-def 'ok))
  internal-def)


#;(let ((a 1))
     (let-syntax
     ;;(letrec-syntax
         ((foo (syntax-rules ()
                 ((_ b)
                  (bar a b))))
          (bar (syntax-rules ()                                                                                                                               ((_ c d)
                  (cons c (let ((c 3))
                            (list d c 'c)))))))
       (let ((a 2))
         (foo a))))


(define-syntax my-let
  (syntax-rules
    ()
    ((my-let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((my-let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))
(write
(my-let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (my-let ((x 'inner))
      (m)))) ;; Should be outer
      )


;; (let-syntax ((given-that (syntax-rules ()
;; ((given-that test stmt1 stmt2 ...)
;; (if test
;; (begin stmt1
;; stmt2 ...))))))
;; (let ((if #t))
;; (given-that if (set! if 'now))
;; if)) ;; => now

(write
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m)))) ;; Should be outer
      )

;(write 
;(let ((x 'outer))
;  (define-syntax m ;; Testing this out, but let-syntax needs to work, too
;   (syntax-rules () ((m) x)))
;    (let ((x 'inner))
;      (m))) ;; Should be outer
;      )
;
;(write (m)) ;; Should be an error, of course
