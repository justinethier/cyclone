(import 
  (scheme base) 
  (scheme write)
)
  (define-syntax match-check-ellipsis
    (er-macro-transformer
     (lambda (expr rename compare)
       (if (compare '... (cadr expr))
           (car (cddr expr))
           (cadr (cddr expr))))))
  (define-syntax match-check-identifier
    (er-macro-transformer
     (lambda (expr rename compare)
       (if (symbol? (cadr expr)) ;; TODO: good enough?
           (car (cddr expr))
           (cadr (cddr expr))))))
(define-syntax match-one
  (syntax-rules ()
    ;; If it's a list of two or more values, check to see if the
    ;; second one is an ellipsis and handle accordingly, otherwise go
    ;; to MATCH-TWO.
    ((match-one v (p q . r) g+s sk fk i)
     (match-check-ellipsis
      q
      (match-extract-vars p (match-gen-ellipsis v p r  g+s sk fk i) i ())
      (match-two v (p q . r) g+s sk fk i)))
    ;; Go directly to MATCH-TWO.
    ((match-one . x)
     (match-two . x))))
(define-syntax match-two
  (syntax-rules (_ ___ ..1 *** quote quasiquote ? $ struct @ object = and or not set! get!)
    ((match-two v () g+s (sk ...) fk i)
     (if (null? v) (sk ... i) fk))
    ((match-two v (quote p) g+s (sk ...) fk i)
     (if (equal? v 'p) (sk ... i) fk))
    ((match-two v (quasiquote p) . x)
     (match-quasiquote v p . x))
    ((match-two v (and) g+s (sk ...) fk i) (sk ... i))
    ((match-two v (and p q ...) g+s sk fk i)
     (match-one v p g+s (match-one v (and q ...) g+s sk fk) fk i))
    ((match-two v (or) g+s sk fk i) fk)
    ((match-two v (or p) . x)
     (match-one v p . x))
    ((match-two v (or p ...) g+s sk fk i)
     (match-extract-vars (or p ...) (match-gen-or v (p ...) g+s sk fk i) i ()))
    ((match-two v (not p) g+s (sk ...) fk i)
     (match-one v p g+s (match-drop-ids fk) (sk ... i) i))
    ((match-two v (get! getter) (g s) (sk ...) fk i)
     (let ((getter (lambda () g))) (sk ... i)))
    ((match-two v (set! setter) (g (s ...)) (sk ...) fk i)
     (let ((setter (lambda (x) (s ... x)))) (sk ... i)))
    ((match-two v (? pred . p) g+s sk fk i)
     (if (pred v) (match-one v (and . p) g+s sk fk i) fk))
    ((match-two v (= proc p) . x)
     (let ((w (proc v))) (match-one w p . x)))
    ((match-two v (p ___ . r) g+s sk fk i)
     (match-extract-vars p (match-gen-ellipsis v p r g+s sk fk i) i ()))
    ((match-two v (p) g+s sk fk i)
     (if (and (pair? v) (null? (cdr v)))
         (let ((w (car v)))
           (match-one w p ((car v) (set-car! v)) sk fk i))
         fk))
    ((match-two v (p *** q) g+s sk fk i)
     (match-extract-vars p (match-gen-search v p q g+s sk fk i) i ()))
    ((match-two v (p *** . q) g+s sk fk i)
     (match-syntax-error "invalid use of ***" (p *** . q)))
    ((match-two v (p ..1) g+s sk fk i)
     (if (pair? v)
         (match-one v (p ___) g+s sk fk i)
         fk))
    ((match-two v ($ rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-refs v rec 0 (p ...) g+s sk fk i)
         fk))
    ((match-two v (struct rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-refs v rec 0 (p ...) g+s sk fk i)
         fk))
    ((match-two v (@ rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-named-refs v rec (p ...) g+s sk fk i)
         fk))
    ((match-two v (object rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-named-refs v rec (p ...) g+s sk fk i)
         fk))
    ((match-two v (p . q) g+s sk fk i)
     (if (pair? v)
         (let ((w (car v)) (x (cdr v)))
           (match-one w p ((car v) (set-car! v))
                      (match-one x q ((cdr v) (set-cdr! v)) sk fk)
                      fk
                      i))
         fk))
    ((match-two v #(p ...) g+s . x)
     (match-vector v 0 () (p ...) . x))
    ((match-two v _ g+s (sk ...) fk i) (sk ... i))
    ;; Not a pair or vector or special literal, test to see if it's a
    ;; new symbol, in which case we just bind it, or if it's an
    ;; already bound symbol or some other literal, in which case we
    ;; compare it with EQUAL?.
    ((match-two v x g+s (sk ...) fk (id ...))
     ;; This extra match-check-identifier is optional in general, but
     ;; can serve as a fast path, and is needed to distinguish
     ;; keywords in Chicken.
     (match-check-identifier
      x
      (let-syntax
          ((new-sym?
            (syntax-rules (id ...)
              ((new-sym? x sk2 fk2) sk2)
              ((new-sym? y sk2 fk2) fk2))))
        (new-sym? random-sym-to-match
                  (let ((x v)) (sk ... (id ... x)))
                  (if (equal? v x) (sk ... (id ...)) fk)))
      (if (equal? v x) (sk ... (id ...)) fk)))
    ))
#;(define-syntax match-two
  (syntax-rules (_ ___ ..1 *** quote quasiquote ? $ struct @ object = my-and or not set! get!)
    ((match-two v () g+s (sk ...) fk i)
     (if (null? v) (sk ... i) fk))
    ((match-two v (quote p) g+s (sk ...) fk i)
     (if (equal? v 'p) (sk ... i) fk))
    ((match-two v (quasiquote p) . x)
     (match-quasiquote v p . x))
    ((match-two v (my-and) g+s (sk ...) fk i) (sk ... i))
    ((match-two v (my-and p q ...) g+s sk fk i)
     (match-one v p g+s (match-one v (my-and q ...) g+s sk fk) fk i))
    ((match-two v (or) g+s sk fk i) fk)
    ((match-two v (or p) . x)
     (match-one v p . x))
    ((match-two v (or p ...) g+s sk fk i)
     (match-extract-vars (or p ...) (match-gen-or v (p ...) g+s sk fk i) i ()))
    ((match-two v (not p) g+s (sk ...) fk i)
     (match-one v p g+s (match-drop-ids fk) (sk ... i) i))
    ((match-two v (get! getter) (g s) (sk ...) fk i)
     (let ((getter (lambda () g))) (sk ... i)))
    ((match-two v (set! setter) (g (s ...)) (sk ...) fk i)
     (let ((setter (lambda (x) (s ... x)))) (sk ... i)))
    ((match-two v (? pred . p) g+s sk fk i)
     (if (pred v) (match-one v (my-and . p) g+s sk fk i) fk))
    ((match-two v (= proc p) . x)
     (let ((w (proc v))) (match-one w p . x)))
    ((match-two v (p ___ . r) g+s sk fk i)
     (match-extract-vars p (match-gen-ellipsis v p r g+s sk fk i) i ()))
    ((match-two v (p) g+s sk fk i)
     (if (and (pair? v) (null? (cdr v)))
         (let ((w (car v)))
           (match-one w p ((car v) (set-car! v)) sk fk i))
         fk))
    ((match-two v (p *** q) g+s sk fk i)
     (match-extract-vars p (match-gen-search v p q g+s sk fk i) i ()))
    ((match-two v (p *** . q) g+s sk fk i)
     (match-syntax-error "invalid use of ***" (p *** . q)))
    ((match-two v (p ..1) g+s sk fk i)
     (if (pair? v)
         (match-one v (p ___) g+s sk fk i)
         fk))
    ((match-two v ($ rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-refs v rec 0 (p ...) g+s sk fk i)
         fk))
    ((match-two v (struct rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-refs v rec 0 (p ...) g+s sk fk i)
         fk))
    ((match-two v (@ rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-named-refs v rec (p ...) g+s sk fk i)
         fk))
    ((match-two v (object rec p ...) g+s sk fk i)
     (if (is-a? v rec)
         (match-record-named-refs v rec (p ...) g+s sk fk i)
         fk))
    ((match-two v (p . q) g+s sk fk i)
     (if (pair? v)
         (let ((w (car v)) (x (cdr v)))
           (match-one w p ((car v) (set-car! v))
                      (match-one x q ((cdr v) (set-cdr! v)) sk fk)
                      fk
                      i))
         fk))
    ((match-two v #(p ...) g+s . x)
     (match-vector v 0 () (p ...) . x))
    ((match-two v _ g+s (sk ...) fk i) (sk ... i))
    ;; Not a pair or vector or special literal, test to see if it's a
    ;; new symbol, in which case we just bind it, or if it's an
    ;; already bound symbol or some other literal, in which case we
    ;; compare it with EQUAL?.
    ((match-two v x g+s (sk ...) fk (id ...))
     ;; This extra match-check-identifier is optional in general, but
     ;; can serve as a fast path, and is needed to distinguish
     ;; keywords in Chicken.
     (match-check-identifier
      x
      (let-syntax
          ((new-sym?
            (syntax-rules (id ...)
              ((new-sym? x sk2 fk2) sk2)
              ((new-sym? y sk2 fk2) fk2))))
        (new-sym? random-sym-to-match
                  (let ((x v)) (sk ... (id ... x)))
                  (if (equal? v x) (sk ... (id ...)) fk)))
      (if (equal? v x) (sk ... (id ...)) fk)))
    ))
;; Takes two values and just expands into the first.
(define-syntax match-drop-ids
  (syntax-rules ()
    ((_ expr ids ...) expr)))


(display
  ;; Works fine with my-and, but change back to and (and above in match-two) and it is broken
  ;(match-two 1 (my-and x) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ())
;; With my-and the initial expansions are:
;/* (expand (match-two 1 (my-and x) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ()))*/ 
;/* (expand (match-one$2095 1 x (1 (set! 1)) (match-one$2095 1 (my-and$2094) (1 (set! 1)) (match-drop-ids (begin x)) (begin)) (begin) ()))*/ 
;/* (expand (match-two$2096 1 x (1 (set! 1)) (match-one$2095 1 (my-and$2094) (1 (set! 1)) (match-drop-ids (begin x)) (begin)) (begin) ()))*/ 
;/* (expand (match-check-identifier$2105 x (let-syntax ((new-sym?$2100 (syntax-rules$2104 () ((new-sym?$2100 x sk2$2102 fk2$2101) sk2$2102) ((new-sym?$2100 y$2103 sk2$2102 fk2$2101) fk2$2101)))) (new-sym?$2100 random-sym-to-match$2099 (let$2098 ((x 1)) (match-one$2095 1 (my-and$2094) (1 (set! 1)) (match-drop-ids (begin x)) (begin) (x))) (if (equal? 1 x) (match-one$2095 1 (my-and$2094) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ()) (begin)))) (if (equal? 1 x) (match-one$2095 1 (my-and$2094) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ()) (begin))))*/ 

  ;; Alternatively, with and the initial expansions are:
  (match-two 1 (and x) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ())
;/* (expand (match-two 1 (and x) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ()))*/ 
;/* (expand (if (pair? 1) (let$2108 ((w$2107 (car 1)) (x$2105 (cdr 1))) (match-one$2106 w$2107 and ((car 1) (set-car! 1)) (match-one$2106 x$2105 (x) ((cdr 1) (set-cdr! 1)) (match-drop-ids (begin x)) (begin)) (begin) ())) (begin)))*/ 
;/* (expand (pair? 1))*/ 
;/* (expand 1)*/ 
;/* (expand pair?)*/ 

;; 
;; All of this seems like it may be a problem with ER macro (compare?)
;; Consider this comparison of and:
;; (Cyc-er-compare? and and$2094)
;; (find-original-sym and #f)
;; (find-original-sym and$2094 #f)
;; (compare and and$2094 and and$2094 #f)
;; 
;; vs the comparison of my-and (which immediately kicks us into the proper macro expansion):
;; 
;; (Cyc-er-compare? my-and my-and$2094)
;; (find-original-sym my-and #f)
;; (find-original-sym my-and$2094 my-and)
;; (find-original-sym my-and #f)
;; (compare my-and my-and$2094 my-and my-and #t)
)
