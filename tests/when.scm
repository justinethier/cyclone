(import (scheme base) (scheme write))
;(define-syntax my-when
;  (syntax-rules ()
;    ((my-when test result1 result2 ...)
;     (if test
;         (begin result1 result2 ...)))))

(define-syntax my-when2
  (syntax-rules ()
    ((my-when test result1 result2 ...)
     (list result2 ...))))

(write
  (my-when2 #t 1))
;
; (define my-when2*
;   (lambda (expr$28 rename$29 compare$30)
;     (car ((lambda (tmp$42)
;             (if tmp$42
;               tmp$42
;               (cons (error "no expansion for" expr$28) #f)))
;           ((lambda (v.1$36)
;              (if (pair? v.1$36)
;                ((lambda (v.2$37)
;                   ((lambda (test)
;                      ((lambda (v.3$38)
;                         (if (pair? v.3$38)
;                           ((lambda (v.4$39)
;                              ((lambda (result1)
;                                 ((lambda (v.5$40)
;                                    (if (list? v.5$40)
;                                      ((lambda (result2)
;                                         (cons (cons-source
;                                                 (rename$29 'list)
;                                                 (cons-source test '() '(test))
;                                                 '(list test))
;                                               #f))
;                                       v.5$40)
;                                      #f))
;                                  (cdr v.3$38)))
;                               v.4$39))
;                            (car v.3$38))
;                           #f))
;                       (cdr v.1$36)))
;                    v.2$37))
;                 (car v.1$36))
;                #f))
;            (cdr expr$28))))))


;; TODO: seems broken
;(define-syntax my-when4
;  (syntax-rules ()
;    ((my-when test result1 result2 ...)
;     (let-syntax 
;       ((second
;         (syntax-rules ()
;           ((second a b c)
;            b))))
;       (second 33 44 55)))))
;(write
;  (my-when4 't 1 2 3))

;; The symbol?? macro from oleg:
;; http://okmij.org/ftp/Scheme/macros.html#macro-symbol-p
     (define-syntax symbol??
       (syntax-rules ()
         ((symbol?? (x . y) kt kf) kf)       ; It's a pair, not a symbol
         ((symbol?? #(x ...) kt kf) kf)      ; It's a vector, not a symbol
         ((symbol?? maybe-symbol kt kf)
           (let-syntax
             ((test
                (syntax-rules ()
                  ((test maybe-symbol t f) t)
                  ((test x t f) f))))
             (test abracadabra kt kf)))))
;;(write (symbol?? a))

(write
  (let-syntax 
    ((second
      (syntax-rules ()
        ((second a b c)
         b))))
    (second 33 44 55)))


;  (my-when2 
;    't 
;    1 
;    (let-syntax 
;      ((my-when3
;        (syntax-rules ()
;          ((my-when3 test result1 result2 ...)
;           (list result2 ...)))))
;      (my-when3 33 44 55))
;    2
;    3))
;(write
;  (my-when2 '(my-when2 't 1 2 3) (lambda (a) a) (lambda X #f)))
;(write
;  (my-when2 '(my-when2 "testing" 1) (lambda (a) a) (lambda X #f)))
