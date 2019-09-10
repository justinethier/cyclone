(import 
  (scheme base)
  (scheme read)
  (scheme cyclone pretty-print)
  (scheme cyclone util))
;; TODO: a temporary file to develop a transform pass that validates the number of arguments passed to
;; built-in forms, and throws an error if any issues are found. The goal is to provide friendlier validation
;; for the compiler
;;
;; TODO: call this from cyclone.scm after it works, probably after "resolve macros"
;; TODO: will relocate this to another place, probably a separate file in scheme/cyclone

(define (validate-syntax exp)
  ;; Only need to track local vars if they shadow one of our keywords
  (define (include-var? v)
    (member v '(define set! if lambda let)))

  (define (check-if exp)
    (let ((args (length exp)))
      (cond
        ((< args 3)
         (error "Not enough arguments" exp))
        ((> args 4)
         (error "Too many arguments" exp)))))

  (define (check-define exp) 
    (let ((args (length exp)))
      (cond
        ((< args 2)
         (error "Not enough arguments" exp)))))

  (define (check-set exp) 
    (let ((args (length exp)))
      (cond
        ((< args 2)
         (error "Not enough arguments" exp)))))

  (define (check-lambda exp) 
    (let ((num-args (length exp)))
      (cond
        ((< num-args 2)
         (error "Not enough arguments" exp))))
    (lambda-formals-type exp)) ;; Validates type information

  ;; TODO: could check primitives, etc

  ;; TODO: what if any keywords are shadowed? need to populate vars
  (define (update-vars syms vars)
    (append vars (filter include-var? syms)))

  (define (search exp vars)
    (pretty-print `(search ,exp ,vars))(newline)
    (cond
      ;((ast:lambda? exp) 'TODO)
      ((const? exp) #f)
      ((quote? exp) #f)
      ((ref? exp)   #f) 
      ((lambda? exp)   
       (if (not (member 'lambda vars)) (check-lambda exp))
        (for-each 
          (lambda (e)
            (search e (update-vars (lambda-formals->list exp) vars)))
          (lambda->exp exp))
      )
      ((and (if? exp)
            (not (member 'if vars)))
       (check-if exp)
       (search (if->condition exp) vars)
       (search (if->then exp) vars)
       (when (> (length exp) 3)
        (search (if->else exp) vars)))
      ((define? exp)
       (if (not (member 'define vars)) (check-define exp))
       (search (define->var exp) vars)
       (for-each
        (lambda (e)
          (search e (update-vars (list (define->var exp)) vars)))
        (define->exp exp)))
      ((define-c? exp) #f)
      ((set!? exp)  
       (if (not (member 'set! vars)) (check-set exp))
       (search (set!->var exp) vars) 
       (search (set!->exp exp) (update-vars (list (set!->var exp)) vars)))
      ; Future?
      ;((tagged-list? 'let exp)
      ; (set! let-vars (append (map car (cadr exp)) let-vars))
      ; (search (cdr exp)))
      ; Application:
      ((app? exp)
       (for-each
        (lambda (e)
          (search e vars))
        exp))
      (else
        (error "unknown expression: " exp))))
  (search exp '()))

;(if)
;(if 1 2 3 4)

 (let ((sexp (read-all (open-input-file "validation.scm"))))
  (validate-syntax sexp))
