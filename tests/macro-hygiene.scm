(import 
  (scheme base) 
  (scheme write) 
  (cyclone test)
  (scheme cyclone pretty-print))

(define (output sexp)
  (write sexp)
  (newline))
 
(test-group
  "basic lambda bindings"
  (test 1 ((lambda (x) x) 1)))

(test-group
  "macro hygiene"
  (test
    'outer
    (let ((x 'outer))
      (let-syntax ((m (syntax-rules () ((m) x))))
        (let ((x 'inner))
          (m)))) ;; Should be outer
          )
  (test
    'outer
    (let ((x 'outer))
      (letrec-syntax ((m (syntax-rules () ((m) x))))
        (let ((x 'inner))
          (m)))) ;; Should be outer
          )
  (test
    'outer
    (let ((x 'outer))
      (define-syntax m ;; Testing this out, but let-syntax needs to work, too
       (syntax-rules () ((m) x)))
        (let ((x 'inner))
          (m))) ;; Should be outer
          )
  (test
    'now
    (let-syntax ((given-that (syntax-rules ()
    ((given-that test stmt1 stmt2 ...)
    (if test
    (begin stmt1
    stmt2 ...))))))
    (let ((if #t))
    (given-that if (set! if 'now))
    if))) ;; => now
  #;(test
    7
   (let ((x #f) 
         (y 7) 
         (temp 8) 
         (my-let odd?) 
         (if even?))
     (or x (my-let temp) (if y) y))) ;; ==> 7
)

;; ;;;; Just testing, may want to remove this one once the recursive macro expansion works
;; ;;;  (define-syntax my-or2 (syntax-rules ()
;; ;;;            ((my-or2) #f)
;; ;;;            ((my-or2 e) e)
;; ;;;            ((my-or2 e1 e2 ...)
;; ;;;             (let ((temp e1)) (if temp temp (my-or2 e2 ...))))))
;; ;;;(write (my-or2 #t))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;;  (define-syntax my-or (syntax-rules ()
;; ;;;            ((my-or) #f)
;; ;;;            ((my-or e) e)
;; ;;;            ((my-or e1 e2 ...)
;; ;;;             (let ((temp e1)) (if temp temp (my-or e2 ...))))))
;; ;;;  (write
;; ;;;  (let ((x #f) 
;; ;;;        (y 7) 
;; ;;;        (temp 8) 
;; ;;;        (my-let odd?) 
;; ;;;        (my-if even?))
;; ;;;    (my-or x (my-let temp) (my-if y) y))) ;; ==> 7
;; ;;;
;; ;;;         (define-syntax foo (syntax-rules ()
;; ;;;                 ((_ b)
;; ;;;                  (bar a b))))
;; ;;;          (define-syntax bar (syntax-rules ()                                                                                                                               ((_ c d)
;; ;;;                  (cons c (let ((c 3))
;; ;;;                            (list d c 'c))))))
;; ;;;       (write
;; ;;;       (let ((a 2))
;; ;;;         (foo a)))
;; ;;
;; ;;;; Chibi also fails with the same error when this is a let-synatx macro,
;; ;;;; so it may be that Cyclone works just fine here! Obviously it needs
;; ;;;; to be able to handle this macro in letrec-syntax form, though
;; ;;#;(let-syntax
;; ;;  ((my-or (syntax-rules ()
;; ;;            ((my-or) #f)
;; ;;            ((my-or e) e)
;; ;;            ((my-or e1 e2 ...)
;; ;;             (let ((temp e1)) (if temp temp (my-or e2 ...)))))))
;; ;;  (let ((x #f) 
;; ;;        (y 7) 
;; ;;        (temp 8) 
;; ;;        (my-let odd?) 
;; ;;        (my-if even?))
;; ;;    (my-or x (my-let temp) (my-if y) y))) ;; ==> 7
;; ;;
;; ;;;; TODO: below should work with "let" and "if" instead of "my-let" and "my-if"
;; ;;;; TODO: below does not work in eval - WTF?
;; (output
;; (letrec-syntax
;;   ((my-or (syntax-rules ()
;;             ((my-or) #f)
;;             ((my-or e) e)
;;             ((my-or e1 e2 ...)
;;              (let ((temp e1)) (if temp temp (my-or e2 ...)))))))
;;   (let ((x #f) 
;;         (y 7) 
;;         (temp 8) 
;;         (my-let odd?) 
;;         (my-if even?))
;;     (my-or x (my-let temp) (my-if y) y))) ;; ==> 7
;; )
;; 
;; ;;
;; ;;;; From Chibi - isn't this a bug though?
;; ;;;(write
;; ;;;(let ()
;; ;;;  (letrec-syntax ()
;; ;;;    (define internal-def 'ok))
;; ;;;  internal-def)
;; ;;;)

(test-group
  "pitfalls and corner cases"

;; ;;;; From Husk:
;; ;;;;
;; ;;; Examples from the source to R5RS pitfall 3.3
;; ;;;; (assert/equal
  (test
   '(1 2 3 a$1385)
   ;'(1 2 3 a)
    (let ((a 1))
       (letrec-syntax
           ((foo (syntax-rules ()
                   ((_ b)
                    (bar a b))))
            (bar (syntax-rules ()
                   ((_ c d)
                    (cons c (let ((c 3))
                              (list d c 'c)))))))
         (let ((a 2))
           (foo a)))))

;;;; ; Examples from/based on pitfall 8.3                                                                                                         (assert/equal 1
  (test
    1
    (let ((x 1))
      (let-syntax ((foo (syntax-rules () ((_) 2))))
        (define x (foo))
        3)
      x))
  (test
    1
    (let ((x 1))
      (letrec-syntax ((foo (syntax-rules () ((_) 2))))                                                                                               (define x (foo))
        3)
      x))

 ;;;; ; Issue #151 - Preserve hygiene across syntax-rules and ER macros
  (test
    '((unquote (quote bar)))
     (let ((unquote 'foo)) `(,'bar))
   )
)

;; ;;
;; ;;#;(let ((a 1))
;; ;;     (let-syntax
;; ;;     ;;(letrec-syntax
;; ;;         ((foo (syntax-rules ()
;; ;;                 ((_ b)
;; ;;                  (bar a b))))
;; ;;          (bar (syntax-rules ()                                                                                                                               ((_ c d)
;; ;;                  (cons c (let ((c 3))
;; ;;                            (list d c 'c)))))))
;; ;;       (let ((a 2))
;; ;;         (foo a))))
;; ;;
;; ;;
;; ;;(define-syntax my-let
;; ;;  (syntax-rules
;; ;;    ()
;; ;;    ((my-let ((name val) ...) body1 body2 ...)
;; ;;     ((lambda (name ...) body1 body2 ...) val ...))
;; ;;    ((my-let tag ((name val) ...) body1 body2 ...)
;; ;;     ((letrec ((tag (lambda (name ...) body1 body2 ...)))
;; ;;        tag)
;; ;;      val ...))))
;; ;;(write
;; ;;(my-let ((x 'outer))
;; ;;  (let-syntax ((m (syntax-rules () ((m) x))))
;; ;;    (my-let ((x 'inner))
;; ;;      (m)))) ;; Should be outer
;; ;;      )
;; ;;
;; ;;

(test-exit)
