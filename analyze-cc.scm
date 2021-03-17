(import
  (scheme base)
  (scheme write)
  (cyclone foreign)
  (srfi 69)
  (scheme cyclone primitives)
  (scheme cyclone transforms)
  (scheme cyclone ast)
  (scheme cyclone cps-optimizations)
  (scheme cyclone util)
  (scheme cyclone libraries))

(define (dump ht)
  (for-each
    (lambda (ref)
      (write ref)
      (newline))
    (hash-table-values ht)))

;;; This function walks over a Closure-converted expression and 
;;; builds a table of all variable references. This can be used 
;;; to determine with certainty what variables are actually used.
;;;
;;; Returns a hash table where each key/var is a referenced var.
(define (analyze-cc-vars sexp)
  (define %ht (make-hash-table))

  (define (add! ref)
    (hash-table-set! %ht ref ref))
  
  (define (scan exp)
    (cond
      ((ast:lambda? exp)
       (scan
        `(%closure ,exp)
         ))
      ((const? exp) #f)
      ((prim?  exp) #f)     
      ((ref?   exp) (add! exp))
      ((quote? exp) #f)
      ((if? exp)
       (scan (if->condition exp))
       (scan (if->then exp))
       (scan (if->else exp)))
      ((tagged-list? '%closure exp)
       (let* ((lam (closure->lam exp))
              (body (car (ast:lambda-body lam))))
         (scan body)))
      ;; Global definition
      ((define? exp)
       (scan (car (define->exp exp))))
      ((define-c? exp)
       #f)
      
      ;; Application:
      ((app? exp)
       (for-each scan exp))
      (else
        (error "unknown exp in analyze-cc-vars " exp))))

  (for-each scan sexp)
  %ht)

(define sexp
'((define char-upcase
   (lambda
     (k$82 c$1$43)
     ((%closure-ref char-lower-case? 0)
      char-lower-case?
      (%closure
        (lambda
          (self$196 r$83)
          (if r$83
            ((%closure-ref (%closure-ref self$196 2) 0)
             (%closure-ref self$196 2)
             (integer->char
               (Cyc-fast-sub
                 (char->integer (%closure-ref self$196 1))
                 32)))
            ((%closure-ref (%closure-ref self$196 2) 0)
             (%closure-ref self$196 2)
             (%closure-ref self$196 1))))
        c$1$43
        k$82)
      c$1$43)))
 (define char-downcase
   (lambda
     (k$91 c$2$44)
     ((%closure-ref char-upper-case? 0)
      char-upper-case?
      (%closure
        (lambda
          (self$197 r$92)
          (if r$92
            ((%closure-ref (%closure-ref self$197 2) 0)
             (%closure-ref self$197 2)
             (integer->char
               (Cyc-fast-plus
                 (char->integer (%closure-ref self$197 1))
                 32)))
            ((%closure-ref (%closure-ref self$197 2) 0)
             (%closure-ref self$197 2)
             (%closure-ref self$197 1))))
        c$2$44
        k$91)
      c$2$44)))
))

(let ((ast (ast:sexp->ast sexp)))
  (dump (analyze-cc-vars ast)))
