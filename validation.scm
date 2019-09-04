;; TODO: a temporary file to develop a transform pass that validates the number of arguments passed to
;; built-in forms, and throws an error if any issues are found. The goal is to provide friendlier validation
;; for the compiler
;;
;; TODO: call this from cyclone.scm after it works, probably after "resolve macros"
;; TODO: will relocate this to another place, probably a separate file in scheme/cyclone

(define (validate-syntax exp)
  'TODO
)

;; free-vars : exp -> sorted-set[var]
;(define (free-vars ast . opts)
;  (define let-vars '())
;  (define bound-only? 
;    (and (not (null? opts))
;         (car opts)))
;
;  (define (search exp)
;    (cond
;      ; Core forms:
;      ((ast:lambda? exp)
;        (difference (reduce union (map search (ast:lambda-body exp)) '())
;                    (ast:lambda-formals->list exp)))
;      ((const? exp)    '())
;      ((quote? exp)    '())    
;      ((ref? exp)      
;       (cond
;        ((prim? exp)     
;         '())    
;        (else
;         (if (member exp let-vars)
;             '()
;             (if bound-only? '() (list exp))))))
;      ((lambda? exp)   
;        (difference (reduce union (map search (lambda->exp exp)) '())
;                    (lambda-formals->list exp)))
;      ((if-syntax? exp)  (union (search (if->condition exp))
;                              (union (search (if->then exp))
;                                     (search (if->else exp)))))
;      ((define? exp)     (union (list (define->var exp))
;                              (search (define->exp exp))))
;      ((define-c? exp) (list (define->var exp)))
;      ((set!? exp)     (union (list (set!->var exp)) 
;                              (search (set!->exp exp))))
;      ((tagged-list? 'let exp)
;       (set! let-vars (append (map car (cadr exp)) let-vars))
;       (search (cdr exp)))
;      ; Application:
;      ((app? exp)       (reduce union (map search exp) '()))
;      (else             (error "unknown expression: " exp))))
;  (search ast))
