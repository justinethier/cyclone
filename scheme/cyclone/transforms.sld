;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module performs Scheme-to-Scheme transformations, and also contains
;;;; various utility functions used by the compiler.
;;;;

(define-library (scheme cyclone transforms)
  (import (scheme base)
          (scheme char)
          (scheme eval)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme cyclone ast)
          (scheme cyclone common)
          (scheme cyclone libraries)
          (scheme cyclone primitives)
          (scheme cyclone pretty-print)
          (scheme cyclone util)
          (srfi 69)
  )
  (export
    *do-code-gen*
    *trace-level*
    *primitives*
    built-in-syms
    trace
    trace:error
    trace:warn
    trace:info
    trace:debug
    cyc:error
    basename
    list-index
    symbol<? 
    insert
    remove 
    union 
    difference 
    reduce 
    azip 
    assq-remove-key 
    assq-remove-keys 
    let? 
    let->bindings 
    let->exp 
    let->bound-vars 
    let->args 
    letrec? 
    letrec->bindings 
    letrec->exp 
    letrec->bound-vars 
    letrec->args 
    lambda-num-args
    ast:lambda-formals-type
    ast:lambda-formals->list
    list->lambda-formals 
    list->pair 
    app->fun 
    app->args 
    precompute-prim-app? 
    begin->exps 
    closure? 
    closure->lam 
    closure->env 
    closure->fv 
    env-make? 
    env-make->id 
    env-make->fields 
    env-make->values 
    env-get? 
    env-get->id 
    env-get->field 
    env-get->env
    set-cell!? 
    set-cell!->cell 
    set-cell!->value 
    cell? 
    cell->value 
    cell-get? 
    cell-get->cell 
    isolate-globals 
    has-global? 
    global-vars 
    filter-unused-variables 
    free-vars 
    clear-mutables
    mark-mutable
    is-mutable? 
    analyze-mutable-variables 
    wrap-mutables 
    alpha-convert 
    cps-convert 
    prim-convert
    validate-keyword-syntax
  )
  (inline
    cell-get->cell
    cell->value
    set-cell!->value
    set-cell!->cell
    env-get->env
    env-get->field
    env-get->id
    env-make->id
    closure->fv
    closure->env
    closure->lam
    begin->exps
    app->args
    app->fun
    letrec->exp
    letrec->bindings
    let->exp
    let->bindings
    void
  )
  (include "pass-validate-syntax.scm")
  (begin

(define (built-in-syms)
  '(call/cc define))

;; Tuning
(define *do-code-gen* #t) ; Generate C code?

;; Trace
(define *trace-level* 2)
(define (trace level msg pp prefix)
    (when (>= *trace-level* level)
        (display "/* ")
        (newline)
        (display prefix)
        (pp msg)
        (display " */")
        (newline)))
(define (trace:error msg) (trace 1 msg pretty-print ""))
(define (trace:warn msg)  (trace 2 msg pretty-print ""))
(define (trace:info msg)  (trace 3 msg pretty-print ""))
(define (trace:debug msg) (trace 4 msg write "DEBUG: "))

(define (cyc:error msg)
  (error msg)
  (exit 1))

;; File Utilities

;; Get the basename of a file, without the extension.
;; EG: "file.scm" ==> "file"
(define (basename filename)
  (let ((pos (list-index #\. (reverse (string->list filename)))))
   (if (= pos -1)
       filename
       (substring filename 0 (- (string-length filename) pos 1)))))

;; Find the first occurence of e within the given list.
;; Returns -1 if e is not found.
(define list-index
  (lambda (e lst)
    (if (null? lst)
      -1
      (if (eq? (car lst) e)
        0
        (if (= (list-index e (cdr lst)) -1) 
          -1
          (+ 1 (list-index e (cdr lst))))))))


;; Utilities.

(cond-expand
  (cyclone
    ; void : -> void
    (define (void) (if #f #t)))
  (else #f))

; symbol<? : symbol symobl -> boolean
;(define (symbol<? sym1 sym2)
;  (string<? (symbol->string sym1)
;            (symbol->string sym2)))

(define-c symbol<?
  "(void *data, int argc, closure _, object k, object sym1, object sym2)"
  "
     Cyc_check_sym(data, sym1);
     Cyc_check_sym(data, sym2);
     object result = (strcmp(symbol_desc(sym1), symbol_desc(sym2)) < 0) 
                     ? boolean_t : boolean_f;
     return_closcall1(data, k, result);
  "
  "(void *data, object ptr, object sym1, object sym2)"
  " 
     Cyc_check_sym(data, sym1);
     Cyc_check_sym(data, sym2);
     object result = (strcmp(symbol_desc(sym1), symbol_desc(sym2)) < 0) 
                     ? boolean_t : boolean_f;
     return result;
  ")

; insert : symbol sorted-set[symbol] -> sorted-set[symbol]
;(define (insert sym S)
;  (if (not (pair? S))
;      (list sym)
;      (cond
;        ((eq? sym (car S))       S)
;        ((symbol<? sym (car S))  (cons sym S))
;        (else (cons (car S) (insert sym (cdr S)))))))
;
(define-c insert
 "(void *data, int argc, closure _,object k_7318, object sym_731_7312, object S_732_7313)"
 "
 pair_type *acc = NULL, *acc_tail = NULL;
 object result;
 while(1) {
  if( (boolean_f != Cyc_is_pair(S_732_7313)) ){
    if( (boolean_f != Cyc_eq(sym_731_7312, Cyc_car(data, S_732_7313))) ){
      //return_closcall1(data,  k_7318,  S_732_7313);
      result = S_732_7313;
      break;
    } else {
      if (strcmp(symbol_desc(sym_731_7312),
                 symbol_desc(Cyc_car(data, S_732_7313))) < 0) {
        //pair_type local_7356;
        //return_closcall1(data,  k_7318,  set_pair_as_expr(&local_7356, sym_731_7312, S_732_7313));
        pair_type* local_7356 = alloca(sizeof(pair_type));
        set_pair(local_7356, sym_731_7312, S_732_7313);
        result = local_7356;
        break;
      } else {
        pair_type *p = alloca(sizeof(pair_type));
        set_pair(p, Cyc_car(data, S_732_7313), NULL);
        if (acc == NULL) {
          acc = p;
          acc_tail = acc;
        } else {
          cdr(acc_tail) = p;
          acc_tail = p;
        }
        S_732_7313 = Cyc_cdr(data, S_732_7313);
        continue;
      }
    }
  } else {
    //pair_type local_7363;
    //return_closcall1(data,  k_7318,  set_cell_as_expr(&local_7363, sym_731_7312));
    pair_type *local_7363 = alloca(sizeof(pair_type));
    set_pair(local_7363, sym_731_7312, NULL);
    result = local_7363;
    break;
  }
}

if (acc) {
  cdr(acc_tail) = result;
  return_closcall1(data, k_7318, (object)acc);
} else {
  return_closcall1(data, k_7318, result);
}
")

; remove : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (remove sym S)
  (if (not (pair? S))
      '()
      (if (eq? (car S) sym)
          (cdr S)
          (cons (car S) (remove sym (cdr S))))))
          
; union : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (union set1 set2)
  ; NOTE: This should be implemented as merge for efficiency.
  (if (not (pair? set1))
      set2
      (insert (car set1) (union (cdr set1) set2))))

; difference : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (difference set1 set2)
  ; NOTE: This can be similarly optimized.
  (if (not (pair? set2))
      set1
      (difference (remove (car set2) set1) (cdr set2))))

; reduce : (A A -> A) list[A] A -> A
(define (reduce f lst init)
  (if (not (pair? lst))
      init
      (reduce f (cdr lst) (f (car lst) init))))

; azip : list[A] list[B] -> alist[A,B]
(define (azip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (list (car list1) (car list2))
            (azip (cdr list1) (cdr list2)))
      '()))

; assq-remove-key : alist[A,B] A -> alist[A,B]
(define (assq-remove-key env key)
  (if (not (pair? env))
      '()
      (if (eq? (car (car env)) key)
          (assq-remove-key (cdr env) key)
          (cons (car env) (assq-remove-key (cdr env) key)))))

; assq-remove-keys : alist[A,B] list[A] -> alist[A,B]
(define (assq-remove-keys env keys)
  (if (not (pair? keys))
      env
      (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))


;; Data type predicates and accessors.

; let? : exp -> boolean
(define (let? exp)
  (tagged-list? 'let exp))

; let->bindings : let-exp -> alist[symbol,exp]
(define (let->bindings exp)
  (cadr exp))

; let->exp : let-exp -> exp
(define (let->exp exp)
  (cddr exp))

; let->bound-vars : let-exp -> list[symbol]
(define (let->bound-vars exp)
  (map car (cadr exp)))

; let->args : let-exp -> list[exp]
(define (let->args exp)
  (map cadr (cadr exp)))

; letrec? : exp -> boolean
(define (letrec? exp)
  (tagged-list? 'letrec exp))

; letrec->bindings : letrec-exp -> alist[symbol,exp]
(define (letrec->bindings exp)
  (cadr exp))

; letrec->exp : letrec-exp -> exp
(define (letrec->exp exp)
  (cddr exp))

; letrec->bound-vars : letrec-exp -> list[symbol]
(define (letrec->bound-vars exp)
  (map car (cadr exp)))

; letrec->args : letrec-exp -> list[exp]
(define (letrec->args exp)
  (map cadr (cadr exp)))

(define (ast:lambda-formals-type ast)
  (lambda-formals-type `(#f ,(ast:lambda-args ast) #f)))

(define (ast:lambda-formals->list ast)
  (lambda-formals->list `(#f ,(ast:lambda-args ast) #f)))

;; Minimum number of required arguments for a lambda
(define (lambda-num-args exp)
  (let ((type (lambda-formals-type exp))
        (num (length (lambda-formals->list exp))))
    (cond
      ((equal? type 'args:varargs)
       -1) ;; Unlimited
      ((equal? type 'args:fixed-with-varargs)
       (- num 1)) ;; Last arg is optional
      (else
        num))))

;; Repack a list of args (symbols) into lambda formals, by type
;; assumes args is a proper list
(define (list->lambda-formals args type)
  (cond 
    ((eq? type 'args:fixed) args)
    ((eq? type 'args:fixed-with-varargs) (list->pair args))
    ((eq? type 'args:varargs) 
     (if (> (length args) 1)
         (error `(Too many args for varargs ,args))
         (car args)))
    (else (error `(Unexpected type ,type)))))

;; Create an improper copy of a proper list
(define (list->pair l)
  (let loop ((lst l))
    (cond
    ((not (pair? lst)) 
     lst)
    ((null? (cdr lst))
     (car lst))
    (else
     (cons (car lst) (loop (cdr lst)))))))

; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))
  
;; Constant Folding
;; Is a primitive being applied in such a way that it can be
;; evaluated at compile time?
(define (precompute-prim-app? ast)
  (and 
    (pair? ast)
    (prim? (car ast))
    (not (prim:udf? (car ast)))
    ;; Does not make sense to precompute these
    (not (member (car ast)
                '(Cyc-global-vars
                  Cyc-get-cvar
                  Cyc-set-cvar!
                  Cyc-cvar?
                  Cyc-opaque?
                  Cyc-spawn-thread!
                  Cyc-end-thread!
                  apply
                  %halt
                  exit
                  system
                  command-line-arguments
                  Cyc-installation-dir
                  Cyc-compilation-environment
                  Cyc-default-exception-handler
                  Cyc-current-exception-handler
                  cell-get
                  set-global!
                  set-global-unsafe!
                  set-cell!
                  cell
                  cons
                  set-car!
                  set-cdr!
                  string-set!
                  string->symbol ;; Could be mistaken for an identifier
                  make-bytevector
                  make-vector
                  ;; I/O must be done at runtime for side effects:
                  Cyc-stdout
                  Cyc-stdin
                  Cyc-stderr
                  open-input-file
                  open-output-file
                  close-port
                  close-input-port
                  close-output-port
                  Cyc-flush-output-port
                  file-exists?
                  delete-file
                  read-char
                  peek-char
                  Cyc-read-line
                  Cyc-write-char
                  Cyc-write
                  Cyc-display)))
    (call/cc
      (lambda (return)
        (for-each
          (lambda (expr)
            (if (or (vector? expr)
                    (not (const? expr)))
              (return #f)))
          (cdr ast))
        #t))))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; closure? : exp -> boolean
(define (closure? exp) 
  (tagged-list? 'closure exp))

; closure->lam : closure-exp -> exp
(define (closure->lam exp) 
  (cadr exp))

; closure->env : closure-exp -> exp
(define (closure->env exp) 
  (caddr exp))

(define (closure->fv exp) 
  (cddr exp))

; env-make? : exp -> boolean
(define (env-make? exp) 
  (tagged-list? 'env-make exp))

; env-make->id : env-make-exp -> env-id
(define (env-make->id exp)
  (cadr exp))

; env-make->fields : env-make-exp -> list[symbol]
(define (env-make->fields exp)
  (map car (cddr exp)))
  
; env-make->values : env-make-exp -> list[exp]
(define (env-make->values exp)
  (map cadr (cddr exp)))

; env-get? : exp -> boolen
(define (env-get? exp)
  (tagged-list? 'env-get exp))

; env-get->id : env-get-exp -> env-id
(define (env-get->id exp)
  (cadr exp))
  
; env-get->field : env-get-exp -> symbol
(define (env-get->field exp)
  (caddr exp))

; env-get->env : env-get-exp -> exp
(define (env-get->env exp)
  (cadddr exp)) 

; set-cell!? : set-cell!-exp -> boolean
(define (set-cell!? exp)
  (tagged-list? 'set-cell! exp))

; set-cell!->cell : set-cell!-exp -> exp
(define (set-cell!->cell exp)
  (cadr exp))

; set-cell!->value : set-cell!-exp -> exp
(define (set-cell!->value exp)
  (caddr exp))

; cell? : exp -> boolean
(define (cell? exp)
  (tagged-list? 'cell exp))

; cell->value : cell-exp -> exp
(define (cell->value exp)
  (cadr exp))

; cell-get? : exp -> boolean
(define (cell-get? exp)
  (tagged-list? 'cell-get exp))

; cell-get->cell : cell-exp -> exp
(define (cell-get->cell exp)
  (cadr exp))

;; Top-level analysis

; Separate top-level defines (globals) from other expressions
;
; This function extracts out non-define statements, and adds them to 
; a "main" after the defines.
;
(define (isolate-globals exp program? lib-name rename-env)
  (let loop ((top-lvl exp)
             (globals '())
             (exprs '()))
    (cond 
      ((null? top-lvl)
       (append
         (reverse globals)
         (expand 
           (cond 
             (program?
               ;; This is the main program, keep top level.
               ;; Use 0 here (and below) to ensure a meaningful top-level
               `((begin 0 ,@(reverse exprs) (%halt 0)))
             )
             (else
               ;; This is a library, keep inits in their own function
               `((define ,(lib:name->symbol lib-name)
                  (lambda () 0 ,@(reverse exprs))))))
           (macro:get-env)
           rename-env)))
      (else
       (cond
         ((define? (car top-lvl))
          (cond
            ;; Global is redefined, convert it to a (set!) at top-level
            ((has-global? globals (define->var (car top-lvl)))
             (loop (cdr top-lvl)
                   globals
                   (cons
                    `(set! ,(define->var (car top-lvl))
                           ,@(define->exp (car top-lvl)))
                     exprs)))
            ;; Form cannot be properly converted to CPS later on, so split it up
            ;; into two parts - use the define to initialize it to false (CPS is fine),
            ;; and place the expression into a top-level (set!), which can be
            ;; handled by the existing CPS conversion.
            ((or 
               ;; TODO: the following line may not be good enough, a global assigned to another
               ;; global may still be init'd to NULL if the order is incorrect in the "top level"
               ;; initialization code.
               (symbol? (car (define->exp (car top-lvl)))) ;; TODO: put these at the end of top-lvl???
               (and (list? (car (define->exp (car top-lvl))))
                    (not (lambda? (car (define->exp (car top-lvl)))))))
             (loop (cdr top-lvl)
                   (cons
                    `(define ,(define->var (car top-lvl)) #f)
                    globals)
                   (cons
                    `(set! ,(define->var (car top-lvl))
                           ,@(define->exp (car top-lvl)))
                    exprs)))
            ;; First time we've seen this define, add it and keep going
            (else
             (loop (cdr top-lvl)
                   (cons (car top-lvl) globals)
                   exprs))))
         ((define-c? (car top-lvl))
          ;; Add as a new global, for now keep things simple
          ;; since this is compiler-specific
          (loop (cdr top-lvl)
                (cons (car top-lvl) globals)
                exprs))
         (else
           (loop (cdr top-lvl)
                 globals
                 (cons (car top-lvl) exprs))))))))

; Has global already been found?
;
; NOTE:
; Linear search may get expensive (n^2), but with a modest set of
; define statements hopefully it will be acceptable. If not, will need
; to use a faster data structure (EG: map or hashtable)
(define (has-global? exp var)
  (call/cc
    (lambda (return)
      (for-each 
        (lambda (e)
          (if (and (define? e)
                   (equal? (define->var e) var))
            (return #t)))
       exp)
      #f)))

; Compute list of global variables based on expression in top-level form
; EG: (def, def, expr, ...)
(define (global-vars exp)
  (let ((globals '()))
    (for-each
      (lambda (e)
        (if (or (define? e)
                (define-c? e))
            (set! globals (cons (define->var e) globals)))
        (if (define-c-inline? e)
            (set! globals (cons (define-c->inline-var e) globals))))
      exp)
    globals))

;; Remove global variables that are not used by the rest of the program.
;; Many improvements can be made, including:
;;
;; TODO: remove unused locals
(define (filter-unused-variables asts lib-exports)
  (define (do-filter code)
    (let ((all-fv ;(apply      ;; More efficient way to do this?
                  ;  append    ;; Could use delete-duplicates
                  (foldr
                    (lambda (l ls)
                      (append ls l))
                   '()
                    (map 
                      (lambda (ast)
                        (if (define? ast)
                            (let ((var (define->var ast)))
                              ;; Do not keep global that refers to itself
                              (filter
                                (lambda (v)
                                  (not (equal? v var)))
                                (free-vars (define->exp ast))))
                            (free-vars ast)))
                      code))))
      (filter
        (lambda (ast)
          (or (not (define? ast))
              (member (define->var ast) all-fv)
              (member (define->var ast) lib-exports)
              (assoc (define->var ast) (get-macros))))
        code)))
  ;; Keep filtering until no more vars are removed
  (define (loop code)
    (let ((new-code (do-filter code)))
      (if (> (length code) (length new-code))
          (loop new-code)
          new-code)))
  (loop asts))

;; Syntactic analysis.

; free-vars : exp -> sorted-set[var]
(define (free-vars ast . opts)
  (define let-vars '())
  (define bound-only? 
    (and (not (null? opts))
         (car opts)))

  (define (search exp)
    (cond
      ; Core forms:
      ((ast:lambda? exp)
        (difference (reduce union (map search (ast:lambda-body exp)) '())
                    (ast:lambda-formals->list exp)))
      ((const? exp)    '())
      ((quote? exp)    '())    
      ((ref? exp)      
       (cond
        ((prim? exp)     
         '())    
        (else
         (if (member exp let-vars)
             '()
             (if bound-only? '() (list exp))))))
      ((lambda? exp)   
        (difference (reduce union (map search (lambda->exp exp)) '())
                    (lambda-formals->list exp)))
      ((if-syntax? exp)  (union (search (if->condition exp))
                              (union (search (if->then exp))
                                     (search (if->else exp)))))
      ((define? exp)     (union (list (define->var exp))
                              (search (define->exp exp))))
      ((define-c? exp) (list (define->var exp)))
      ((set!? exp)     (union (list (set!->var exp)) 
                              (search (set!->exp exp))))
      ((tagged-list? 'let exp)
       (set! let-vars (append (map car (cadr exp)) let-vars))
       (search (cdr exp)))
      ; Application:
      ((app? exp)       (reduce union (map search exp) '()))
      (else             (error "unknown expression: " exp))))
  (search ast))





;; Mutable variable analysis and elimination.

;; Mutables variables analysis and elimination happens
;; on a desugared Intermediate Language (1).

;; Mutable variable analysis turns mutable variables 
;; into heap-allocated cells:

;; For any mutable variable mvar:

;; (lambda (... mvar ...) body) 
;;           =>
;; (lambda (... $v ...) 
;;  (let ((mvar (cell $v)))
;;   body))

;; (set! mvar value) => (set-cell! mvar value)

;; mvar => (cell-get mvar)

; mutable-variables : list[symbol]
(define mutable-variables '())

(define (clear-mutables)
  (set! mutable-variables '()))

; mark-mutable : symbol -> void
(define (mark-mutable symbol)
  (set! mutable-variables (cons symbol mutable-variables)))

; is-mutable? : symbol -> boolean
(define (is-mutable? symbol)
  (define (is-in? S)
    (if (not (pair? S))
        #f
        (if (eq? (car S) symbol)
            #t
            (is-in? (cdr S)))))
  (is-in? mutable-variables))

; analyze-mutable-variables : exp -> void
(define (analyze-mutable-variables exp)
  (cond 
    ; Core forms:
    ((ast:lambda? exp)
     (map analyze-mutable-variables (ast:lambda-body exp))
     (void))
    ((const? exp)    (void))
    ((prim? exp)     (void))
    ((ref? exp)      (void))
    ((quote? exp)    (void))
    ((lambda? exp)   
     (map analyze-mutable-variables (lambda->exp exp))
     (void))
    ((set!? exp)     
     (mark-mutable (set!->var exp))
     (analyze-mutable-variables (set!->exp exp)))
    ((if? exp)       
     (analyze-mutable-variables (if->condition exp))
     (analyze-mutable-variables (if->then exp))
     (analyze-mutable-variables (if->else exp)))
    ; Application:
    ((app? exp)
     (map analyze-mutable-variables exp)
     (void))
    (else
     (error "unknown expression type: " exp))))


; wrap-mutables : exp -> exp
(define (wrap-mutables exp globals)
  
  (define (wrap-mutable-formals id formals body-exp has-cont)
    (if (not (pair? formals))
        body-exp
        ;(list body-exp)
        (if (is-mutable? (car formals))
            (list
              (list ;(ast:%make-lambda
                    ;  id
                    (ast:make-lambda
                      (list (car formals))
                      (wrap-mutable-formals id (cdr formals) body-exp has-cont)
                      has-cont)
                    `(cell ,(car formals))))
            (wrap-mutable-formals id (cdr formals) body-exp has-cont))))
  
  (cond
    ; Core forms:
    ((ast:lambda? exp)
     (ast:%make-lambda
       (ast:lambda-id exp)
       (ast:lambda-args exp)
       (wrap-mutable-formals 
         (ast:lambda-id exp)
         (ast:lambda-formals->list exp)
         (list (wrap-mutables (car (ast:lambda-body exp)) globals))
         (ast:lambda-has-cont exp))
       (ast:lambda-has-cont exp)
       )) ;; Assume single expr in lambda body, since after CPS phase
    ((const? exp)    exp)
    ((ref? exp)      (if (and (not (member exp globals))
                              (is-mutable? exp))
                         `(cell-get ,exp)
                         exp))
    ((prim? exp)     exp)
    ((quote? exp)    exp)
    ((lambda? exp)   (error `(Unexpected lambda in wrap-mutables ,exp)))
    ((set!? exp)   
     (cond
       ((member (set!->var exp) globals)
          `(set-global!
            ,(list 'quote (set!->var exp))
            ,(set!->var exp) 
            ,(wrap-mutables (set!->exp exp) globals)) )
      (else
          `(set-cell!
            ,(set!->var exp) 
            ,(wrap-mutables (set!->exp exp) globals))) ))
    ((if? exp)       `(if ,(wrap-mutables (if->condition exp) globals)
                          ,(wrap-mutables (if->then exp) globals)
                          ,(wrap-mutables (if->else exp) globals)))
    
    ; Application:
    ((app? exp)
     ;; Easy place to clean up nested Cyc-seq expressions
     (when (tagged-list? 'Cyc-seq exp)
           (set! exp (flatten-sequence exp)))
     (let ((result (map (lambda (e) (wrap-mutables e globals)) exp)))
       ;; This code can eliminate a lambda definition. But typically
       ;; the code that would have such a definition has a recursive
       ;; inner loop, so there is not much savings to eliminating the
       ;; single outer lambda:
       ;;
       ;;(cond
       ;; ((and (lambda? (car result))
       ;;       (equal? (cdr result) '(#f))
       ;;       (app? (car (lambda->exp (car result))))
       ;;       (lambda? (car (car (lambda->exp (car result))))))
       ;;  (let* ((inner-lambda (car (car (lambda->exp (car result)))))
       ;;         (inner-formals (lambda-formals->list inner-lambda))
       ;;         (inner-args (cdr (car (lambda->exp (car result)))))
       ;;         (outer-formals (lambda-formals->list (car result)))
       ;;         (opt? (and (pair? outer-formals)
       ;;                    (is-mutable? (car outer-formals))
       ;;                    (equal? outer-formals inner-formals)
       ;;                    (equal? inner-args `((cell ,(car inner-formals))))
       ;;                    )))
       ;;    (trace:error `(DEBUG ,opt? ,outer-formals ,inner-formals ,inner-args))
       ;;    ;result
       ;;    (if opt?
       ;;        `(,inner-lambda (cell #f))
       ;;        result)
       ;; ))
       ;; (else result))))
       result))
    (else            (error "unknown expression type: " exp))))

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
(define (flatten-sequence sexp)
  (define (flat sexp acc)
    (cond
      ((not (pair? sexp)) ;; Stop at end of sexp
       acc)
      ((and (tagged-list? 'Cyc-seq (car sexp))) ;; Flatten nexted sequences
        (flat (cdar sexp) acc))
      ((and (ref? (car sexp)) ;; Remove unused identifiers
            (not (equal? 'Cyc-seq (car sexp))))
        (flat (cdr sexp) acc))
      (else ;;(pair? sexp)
        (flat (cdr sexp) (cons (car sexp) acc))))
  )
  (reverse
    (flat sexp '())))


;; Alpha conversion
;; (aka alpha renaming)
;;
;; This phase is intended to rename identifiers to preserve lexical scoping
;;
;; TODO: does not properly handle renaming builtin functions, would probably need to
;; pass that renaming information downstream
(define (alpha-convert ast globals return-unbound)
  ;; Initialize top-level variables
  (define (initialize-top-level-vars ast fv)
    (if (> (length fv) 0)
       ;; Free variables found, set initial values
       `((lambda ,fv ,ast)
          ,@(map (lambda (_) #f) fv))
        ast))

  ;; Find any defined variables in the given code block
  (define (find-defined-vars ast)
    (filter
      (lambda (expr)
        (not (null? expr)))
      (map
        (lambda (expr)
          (if (define? expr)
            (define->var expr)
           '()))
        ast)))

  ;; Take a list of identifiers and generate a list of 
  ;; renamed pairs, EG: (var . renamed-var)
  (define (make-a-lookup vars)
    (map 
      (lambda (a) (cons a (gensym a))) 
      vars))

  ;; Wrap any defined variables in a lambda, so they can be initialized
  (define (initialize-defined-vars ast vars)
    (if (> (length vars) 0)
      `(((lambda ,vars ,@ast)
         ,@(map (lambda (_) #f) vars)))
       ast))

  ;; Perform actual alpha conversion
  (define (convert ast renamed)
    (cond
      ((const? ast) ast)
      ((quote? ast) ast)
      ((ref? ast)
       (let ((renamed (assoc ast renamed)))
         (cond
          (renamed 
            (cdr renamed))
          (else ast))))
      ((and (define? ast)
            (not (assoc 'define renamed)))
       ;; Only internal defines at this point, of form: (define ident value)
       `(set! ,@(map (lambda (a) (convert a renamed)) (cdr ast))))
      ((and (set!? ast)
            (not (assoc 'set! renamed)))
         ;; Without define, we have no way of knowing if this was a
         ;; define or a set prior to this phase. But no big deal, since
         ;; the set will still work in either case, so no need to check
         `(set! ,@(map (lambda (a) (convert a renamed)) (cdr ast))))
      ((and (if? ast)
            (not (assoc 'if renamed)))
       ;; Add a failsafe here in case macro expansion added more
       ;; incomplete if expressions.
       (let ((new-ast (if (if-else? ast)
                          `(if ,@(map (lambda (a) (convert a renamed)) (cdr ast)))
                          (convert (append ast (list (void))) renamed))))
         (cond
          ;; Optimization - convert (if (not a) b c) into (if a c b)
          ((and (app? (if->condition new-ast))
                (equal? 'not (app->fun (if->condition new-ast))))
           `(if ,@(app->args (if->condition new-ast))
                ,(if->else new-ast)
                ,(if->then new-ast)))
          ;; Optimization - convert (if expr #t #f) into expr
          ((and (eq? #t (if->then new-ast))
                (eq? #f (if->else new-ast))
                (app? (if->condition new-ast))
                (member 
                  (car (if->condition new-ast)) 
                  '(Cyc-fast-eq
                    Cyc-fast-gt
                    Cyc-fast-lt
                    Cyc-fast-gte
                    Cyc-fast-lte
                    Cyc-fast-char-eq
                    Cyc-fast-char-gt
                    Cyc-fast-char-lt
                    Cyc-fast-char-gte
                    Cyc-fast-char-lte
                    eq?
                    eqv?
                    equal?
                    boolean?
                    char?
                    eof-object?
                    null?
                    number?
                    real?
                    integer?
                    pair?
                    port?
                    procedure?
                    Cyc-macro?
                    vector?
                    string?
                    symbol?
                    =
                    >
                    <
                    >=
                    <=))) ;; Boolean return
           (if->condition new-ast))
          (else
            new-ast))))
      ((and (prim-call? ast)
            ;; Not a primitive if the identifier has been redefined
            (not (assoc (car ast) renamed)))
       (let ((converted
               (cons (car ast) 
                     (map (lambda (a) (convert a renamed))
                          (cdr ast)))))
         (cond
          ((and (equal? (car converted) '+) (= (length converted) 1))
           0)
          ((and (equal? (car converted) '*) (= (length converted) 1))
           1)
          ((precompute-prim-app? converted)
           converted) ; TODO:(eval converted) ;; OK, evaluate at compile time
           ;converted))) ;; No, see if we can fast-convert it
          (else
           (prim:inline-convert-prim-call converted))))) ;; No, see if we can fast-convert it
      ((and (lambda? ast)
            (not (assoc 'lambda renamed)))
       (let* ((args (lambda-formals->list ast))
              (ltype (lambda-formals-type ast))
              (a-lookup (map (lambda (a) (cons a (gensym a))) args))
              (body (lambda->exp ast))
              (define-vars (find-defined-vars body))
              (defines-a-lookup (make-a-lookup define-vars))
             )
         ;; This is a convenient place to check for duplicate lambda args
         (if (not (equal? (delete-duplicates args) args))
             (error "duplicate lambda parameter(s)" args))
         ;; New lambda code
         `(lambda 
            ,(list->lambda-formals
                (map (lambda (p) (cdr p)) a-lookup)  
                ltype)
            ,@(initialize-defined-vars 
                (convert 
                    body 
                   (append a-lookup defines-a-lookup renamed))
                (map (lambda (p) (cdr p)) defines-a-lookup)))))
      ((app? ast)
       (let ((regular-case 
               (lambda ()
                 ;; Regular case, alpha convert everything
                 (map (lambda (a) (convert a renamed)) ast))))
         (cond
          ;; If identifier is renamed it is not a special case
          ((assoc (car ast) renamed)
           (regular-case))
          ;; Special case, convert these to primitives if possible
          ((and (eq? (car ast) 'member) (= (length ast) 3))
           (cons 'Cyc-fast-member (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'assoc) (= (length ast) 3))
           (cons 'Cyc-fast-assoc (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'vector) (= (length ast) 3))
           (cons 'Cyc-fast-vector-2 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'vector) (= (length ast) 4))
           (cons 'Cyc-fast-vector-3 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'vector) (= (length ast) 5))
           (cons 'Cyc-fast-vector-4 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'vector) (= (length ast) 6))
           (cons 'Cyc-fast-vector-5 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'list) (= (length ast) 2))
           (cons 'Cyc-fast-list-1 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'list) (= (length ast) 3))
           (cons 'Cyc-fast-list-2 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'list) (= (length ast) 4))
           (cons 'Cyc-fast-list-3 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'list) (= (length ast) 5))
           (cons 'Cyc-fast-list-4 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'for-each) (= (length ast) 3))
           (cons 'Cyc-for-each-loop-1 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'for-each) (= (length ast) 4))
           (cons 'Cyc-for-each-loop-2 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'map) (= (length ast) 3))
           (cons 'Cyc-map-loop-1 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'map) (= (length ast) 4))
           (cons 'Cyc-map-loop-2 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'write-string) (= (length ast) 2))
           (cons 'write-string-1 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'write-string) (= (length ast) 3))
           (cons 'write-string-2 (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'string>=?) (= (length ast) 3))
           (cons 'fast-string>=? (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'string>?) (= (length ast) 3))
           (cons 'fast-string>? (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'string<=?) (= (length ast) 3))
           (cons 'fast-string<=? (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'string<?) (= (length ast) 3))
           (cons 'fast-string<? (map (lambda (a) (convert a renamed)) (cdr ast))))
          ((and (eq? (car ast) 'string=?) (= (length ast) 3))
           (cons 'fast-string=? (map (lambda (a) (convert a renamed)) (cdr ast))))
          ;; Regular case, alpha convert everything
          (else
           (regular-case)))))
      (else
        (error "unhandled expression: " ast))))

  (let* ((fv (difference (free-vars ast) globals))
         ;; Only find set! and lambda vars
         (bound-vars (union (free-vars ast #t) globals))
         ;; vars never bound in prog, but could be built-in
         (unbound-vars (difference fv bound-vars))
         ;; vars we know nothing about - error!
         (unknown-vars (difference unbound-vars (built-in-syms)))
         )
    (cond
     ((> (length unknown-vars) 0)
      (let ((unbound-to-return (list)))
        ;; Legacy? Should not be any reason to return early at this point
        ;(if (member 'eval unknown-vars) 
        ;    (set! unbound-to-return (cons 'eval unbound-to-return)))
        ;(if (or (member 'read unknown-vars) 
        ;        (member 'read-all unknown-vars))
        ;    (set! unbound-to-return (cons 'read unbound-to-return)))
        (if (and (> (length unbound-to-return) 0) 
                 (= (length unknown-vars) (length unbound-to-return)))
            (return-unbound unbound-to-return)
            ;; TODO: should not report above (eval read) as errors
            (error "Unbound variable(s)" unknown-vars))))
     ((define? ast)
      ;; Deconstruct define so underlying code can assume internal defines
      (let ((body (car ;; Only one member by now
                    (define->exp ast))))
;(write `(DEBUG body ,body))
        (cond
          ((lambda? body)
           (let* ((args (lambda-formals->list body))
                  (ltype (lambda-formals-type body))
                  (a-lookup (map (lambda (a) (cons a (gensym a))) args))
                  (define-vars (find-defined-vars (lambda->exp body)))
                  (defines-a-lookup (make-a-lookup define-vars))
                 )
            ;; Any internal defines need to be initialized within the lambda,
            ;; so the lambda formals are preserved. So we need to deconstruct
            ;; the defined lambda and then reconstruct it, with #f placeholders
            ;; for any internal definitions.
            ;;
            ;; Also, initialize-top-level-vars cannot be used directly due to 
            ;; the required splicing.
            `(define 
               ,(define->var ast)
               (lambda
                 ,(list->lambda-formals
                    (map (lambda (p) (cdr p)) a-lookup)  
                    ltype)
                 ,@(convert (let ((fv* (union
                                         define-vars
                                         (difference fv (built-in-syms))))
                                 (ast* (lambda->exp body)))
                             (if (> (length fv*) 0)
                               `(((lambda ,fv* ,@ast*)
                                  ,@(map (lambda (_) #f) fv*)))
                                ast*))
                            (append a-lookup defines-a-lookup))))))
          (else
            `(define 
               ,(define->var ast)
               ,@(convert (initialize-top-level-vars 
                            (define->exp ast)
                            (difference fv (built-in-syms)))
                          (list)))))))
     (else
      (convert (initialize-top-level-vars 
                 ast 
                 (difference fv (built-in-syms)))
               (list))))))

;; Upgrade applicable function calls to inlinable primitives
;;
;; This must execute after alpha conversion so that any locals
;; are renamed and if expressions always have an else clause.
;;
(define (prim-convert expr)
  (define (conv ast)
    (cond
      ((const? ast) ast)
      ((quote? ast) ast)
      ((ref? ast)   ast)
      ((define? ast)
       `(define
          ,(cadr ast) ;; Preserve var/args
          ,@(map conv (define->exp ast))))
      ((set!? ast)
       `(set! ,@(map (lambda (a) (conv a)) (cdr ast))))
      ((set!? ast)
       `(set! ,@(map (lambda (a) (conv a)) (cdr ast))))
      ((if? ast)
       `(if ,(conv (if->condition ast))
            ,(conv (if->then ast))
            ,(conv (if->else ast))))
      ((lambda? ast)
       (let* ((args (lambda-formals->list ast))
              (ltype (lambda-formals-type ast))
              (body (lambda->exp ast))
            )
         `(lambda
            ,(list->lambda-formals args ltype) ;; Overkill??
            ,@(map conv body))))
      ((app? ast)
       (cond
        ((ref? (car ast))
         `( ,(prim:func->prim (car ast) (- (length ast) 1))
            ,@(map conv (cdr ast))))
        (else
         (map conv ast))))
      (else
        ast)))
  (conv expr))

;;
;; Helpers to syntax check primitive calls
;;
(define *prim-args-table*
  (alist->hash-table *primitives-num-args*))

;; CPS conversion 
;;
;; This is a port of code from the 90-minute Scheme->C Compiler by Marc Feeley
;;
;; Convert intermediate code to continuation-passing style, to allow for
;; first-class continuations and call/cc
;;

(define (cps-convert ast)

  (define (cps ast cont-ast)
    (cond
          ((const? ast)
           (list cont-ast ast))

          ((ref? ast)
           (list cont-ast ast))

          ((quote? ast)
           (list cont-ast ast))

          ((set!? ast)
           (cps-list (cddr ast) ;; expr passed to set
                     (lambda (val)
                       (list cont-ast 
                         `(set! ,(cadr ast) ,@val))))) ;; cadr => variable

          ((if? ast)
           (let ((xform
                  (lambda (cont-ast)
                    (cps-list (list (cadr ast))
                              (lambda (test)
                                 (list 'if
                                       (car test)
                                       (cps (caddr ast)
                                            cont-ast)
                                       (cps (cadddr ast)
                                            cont-ast)))))))
             (if (ref? cont-ast) ; prevent combinatorial explosion
                 (xform cont-ast)
                 (let ((k (gensym 'k)))
                    (list (ast:make-lambda
                           (list k)
                           (list (xform k))
                           #t)
                          cont-ast)))))

          ((prim-call? ast)
           (prim:check-arg-count
                 (car ast)
                 (- (length ast) 1)
                 (hash-table-ref/default 
                   *prim-args-table*
                   (car ast)
                   #f))
           (cps-list (cdr ast) ; args to primitive function
                     (lambda (args)
                        (list cont-ast
                            `(,(car ast) ; op
                              ,@args)))))

          ((lambda? ast)
           (let ((k (gensym 'k))
                 (ltype (lambda-formals-type ast)))
             (list cont-ast
                   (ast:make-lambda
                      (list->lambda-formals
                         (cons k (cadr ast)) ; lam params
                         (if (equal? ltype 'args:varargs)
                             'args:fixed-with-varargs ;; OK? promote due to k
                             ltype))
                      (list (cps-seq (cddr ast) k)) 
                      #t))))

;; TODO: add test cases
;(define (test)
;  ((lambda (a . Y) (write Y)) 'x)
;  ((lambda (a . Y) (write Y)) 'x 'y)
;  ((lambda (a . Y) (write Y)) 'x 'y 'z)
;  ((lambda (a b . Y) (write Y)) 'x 'y 'z)
;  ((lambda Y (write Y)) 'x 'y 'z)
;    (lambda X (list X)))

          ((and (app? ast)
                (lambda? (app->fun ast))
                (equal? 'args:fixed-with-varargs (lambda-formals-type (app->fun ast))))
           (let ((lam-min-num-args (lambda-num-args (app->fun ast)))
                 (num-args (length (app->args ast)))
                 (ltype (lambda-formals-type (app->fun ast))))
            (cond
             ((< num-args lam-min-num-args)
              (error 
                (string-append
                  "Not enough arguments passed to anonymous lambda. "
                  "Expected "
                  (number->string lam-min-num-args)
                  " but received "
                  (number->string num-args))
                (app->fun ast)))))
           (let* ((fn (app->fun ast))
                  (formals (lambda->formals fn))
                  (formals-lis (pair->list formals)) ;; Formals as proper list
                  (formals-len (length formals-lis))
                  (req-args (take (cdr ast) (- formals-len 1)))
                  (opt-args (drop (cdr ast) (- formals-len 1)))
                 )
             ;; Special case, rewrite into a "normal" lambda and try again
             (cps `((lambda 
                      ,formals-lis
                      ,@(cddr fn))
                    ,@req-args
                    (list ,@opt-args))
                  cont-ast) ))

          ((and (app? ast)
                (lambda? (app->fun ast))
                (equal? 'args:varargs (lambda-formals-type (app->fun ast))))
           (let ((fn (app->fun ast)))
             ;; Special case, rewrite into a "normal" lambda and try again
             (cps `((lambda 
                      (,(cadr fn)) 
                      ,@(cddr fn))
                    (list ,@(cdr ast)))
                  cont-ast) ))

          ((app? ast)
           ;; Syntax check the function
           (if (const? (car ast))
               (error "Call of non-procedure: " ast))
           ;; Do conversion
           (let ((fn (app->fun ast)))
             (cond
              ((lambda? fn)
               ;; Check number of arguments to the lambda
               (let ((lam-min-num-args (lambda-num-args fn))
                     (num-args (length (app->args ast)))
                     (ltype (lambda-formals-type fn)))
                (cond
                 ((< num-args lam-min-num-args)
                  (error 
                    (string-append
                      "Not enough arguments passed to anonymous lambda. "
                      "Expected "
                      (number->string lam-min-num-args)
                      " but received "
                      (number->string num-args))
                    fn))
                 ((and (> num-args lam-min-num-args)
                       (equal? 'args:fixed ltype))
                  (error 
                    (string-append
                      "Too many arguments passed to anonymous lambda. "
                      "Expected "
                      (number->string lam-min-num-args)
                      " but received "
                      (number->string num-args))
                    fn)))
                ;; Do conversion
                (cps-list (app->args ast)
                          (lambda (vals)
                            (cons (ast:make-lambda
                                    (lambda->formals fn)
                                    (list (cps-seq (cddr fn) ;(ast-subx fn)
                                                   cont-ast)))
                                   vals)))))
              (else
                 (cps-list ast ;(ast-subx ast)
                           (lambda (args)
                              (cons (car args)
                                    (cons cont-ast
                                          (cdr args)))))))))

          (else
           (error "unknown ast" ast))))

  (define (cps-list asts inner)
    (define (body x)
      (cps-list (cdr asts)
                (lambda (new-asts)
                  (inner (cons x new-asts)))))

    (cond ((null? asts)
           (inner '()))
          ((or (const? (car asts))
               (ref? (car asts)))
           (body (car asts)))
          (else
           (let ((r (gensym 'r))) ;(new-var 'r)))
             (cps (car asts)
                  (ast:make-lambda (list r) (list (body r))))))))

  (define (cps-seq asts cont-ast)
    (cond ((null? asts)
           (list cont-ast #f))
          ((null? (cdr asts))
           (cps (car asts) cont-ast))
          (else
           (let ((r (gensym 'r)))
             (cps (car asts)
                  (ast:make-lambda
                     (list r)
                     (list (cps-seq (cdr asts) cont-ast))))))))

  ;; Remove dummy symbol inserted into define forms converted to CPS
  (define (remove-unused ast)
    (list (car ast) (cadr ast) (cadddr ast)))

  (let* ((global-def? (define? ast)) ;; No internal defines by this phase
         (ast-cps
          (cond
           (global-def?
            (remove-unused
              `(define ,(define->var ast)
                ,@(let ((k (gensym 'k))
                        (r (gensym 'r)))
                   (cps (car (define->exp ast)) 'unused)))))
            ((define-c? ast)
             ast)
            (else
              (cps ast '%halt)))))
    ast-cps))

; Suitable definitions for the cell functions:
;(define (cell value) (lambda (get? new-value) 
;                       (if get? value (set! value new-value))))
;(define (set-cell! c v) (c #f v))
;(define (cell-get c) (c #t #t))

))
