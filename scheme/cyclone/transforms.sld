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
          (scheme file)
          (scheme eval)
          (scheme read)
          (scheme write)
          (scheme cyclone common)
          (scheme cyclone util)
          (scheme cyclone libraries)
          (scheme cyclone macros)
  )
  (export
    *defined-macros* 
    *do-code-gen*
    *trace-level*
    *primitives*
    get-macros
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
    const? 
    ref? 
    quote? 
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
    lambda? 
    lambda-varargs? 
    lambda->formals 
    lambda-varargs? 
    lambda-varargs-var 
    lambda-formals-type 
    lambda-formals->list 
    lambda-num-args
    list->lambda-formals 
    pair->list 
    list->pair 
    lambda->exp 
    if? 
    if->condition 
    if->then 
    if-else? 
    if->else 
    app? 
    app->fun 
    app->args 
    prim? 
    precompute-prim-app? 
    prim-call? 
    begin? 
    begin->exps 
    define? 
    define-lambda? 
    define->lambda 
    define->var 
    define->exp 
    define-c?
    set!? 
    set!->var 
    set!->exp 
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
    expand 
    expand-lambda-body
    let=>lambda 
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
    cps-optimize-01
    pos-in-list 
    closure-convert 
  )
  (begin

;; Temporary work-around for pp not being implemented yet
(define pretty-print write)

;; Container for built-in macros
(define (get-macros) *defined-macros*)
(define *defined-macros* (list))

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
(define (trace:debug msg) (trace 4 msg display "DEBUG: "))

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
    ; member : symbol sorted-set[symbol] -> boolean
    (define (member sym S)
      (if (not (pair? S))
          #f
          (if (eq? sym (car S))
              #t
              (member sym (cdr S))))))
  (else #f))

(cond-expand
  (cyclone
    ; void : -> void
    (define (void) (if #f #t)))
  (else #f))

; symbol<? : symbol symobl -> boolean
(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1)
            (symbol->string sym2)))

; insert : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (insert sym S)
  (if (not (pair? S))
      (list sym)
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        (else (cons (car S) (insert sym (cdr S)))))))

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

; const? : exp -> boolean
(define (const? exp)
  (or (integer? exp)
      (real? exp)
      (string? exp)
      (vector? exp)
      (char? exp)
      (boolean? exp)))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; quote? : exp -> boolean
(define (quote? exp)
  (tagged-list? 'quote exp))

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

(define (lambda-varargs? exp)
  (and (lambda? exp)
       (or (symbol? (lambda->formals exp))
           (and (pair? (lambda->formals exp))
                (not (list? (lambda->formals exp)))))))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

(define (lambda-varargs? exp)
  (let ((type (lambda-formals-type exp)))
    (or (equal? type 'args:varargs)
        (equal? type 'args:fixed-with-varargs))))

(define (lambda-varargs-var exp)
  (if (lambda-varargs? exp)
    (if (equal? (lambda-formals-type exp) 'args:varargs)
        (lambda->formals exp) ; take symbol directly
        (car (reverse (lambda-formals->list exp)))) ; Last arg is varargs
    #f))

(define (lambda-formals-type exp)
 (let ((args (lambda->formals exp)))
   (cond
     ((symbol? args) 'args:varargs)
     ((list? args)   'args:fixed)
     ((pair? args)   'args:fixed-with-varargs)
     (else
       (error `(Unexpected formals list in lambda-formals-type: ,args))))))

(define (lambda-formals->list exp)
  (if (lambda-varargs? exp)
      (let ((args (lambda->formals exp)))
        (if (symbol? args)
            (list args)
            (pair->list args)))
      (lambda->formals exp)))

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

;; Create a proper copy of an improper list
;; EG: (1 2 . 3) ==> (1 2 3)
(define (pair->list p)
  (let loop ((lst p))
    (if (not (pair? lst))
        (cons lst '())
        (cons (car lst) (loop (cdr lst))))))

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

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (cddr exp)) ;; JAE - changed from caddr, so we can handle multiple expressions

; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

;; if-else? : if-exp -> bool
;; Determines whether an if expression has an else clause
(define (if-else? exp)
  (and (tagged-list? 'if exp)
       (> (length exp) 3)))

; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))

; app? : exp -> boolean
(define (app? exp)
  (pair? exp))

; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))
  
; prim? : exp -> boolean
(define (prim? exp)
  (member exp *primitives*))

(define *primitives* '(
     Cyc-global-vars
     Cyc-get-cvar
     Cyc-set-cvar!
     Cyc-cvar? ;; Cyclone-specific
     Cyc-has-cycle?
     Cyc-spawn-thread!
     Cyc-end-thread!
     Cyc-stdout
     Cyc-stdin
     Cyc-stderr
     +
     -
     *
     /
     =
     >
     <
     >=
     <=
     apply
     %halt
     exit
     system
     command-line-arguments
     Cyc-installation-dir
     Cyc-default-exception-handler
     Cyc-current-exception-handler
     cons
     cell-get
     set-global!
     set-cell!
     cell
     eq?
     eqv?
     equal?
     assoc
     assq
     assv
     memq
     memv
     member
     length
     set-car!
     set-cdr!
     car
     cdr
     caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr
     caaaar caaadr caadar caaddr cadaar cadadr
     caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
     char->integer
     integer->char
     string->number
     string-append
     string-cmp
     list->string
     string->symbol
     symbol->string
     number->string
     string-length
     string-ref
     string-set!
     substring
     make-bytevector
     bytevector-length
     bytevector
     bytevector-append
     bytevector-u8-ref
     bytevector-u8-set!
     bytevector?
     make-vector
     list->vector
     vector-length
     vector-ref
     vector-set!
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
     macro?
     vector?
     string?
     symbol?
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
     Cyc-display))

;; Constant Folding
;; Is a primitive being applied in such a way that it can be
;; evaluated at compile time?
(define (precompute-prim-app? ast)
  (and 
    (pair? ast)
    (prim? (car ast))
    ;; Does not make sense to precompute these
    (not (member (car ast)
                '(Cyc-global-vars
                  Cyc-get-cvar
                  Cyc-set-cvar!
                  Cyc-cvar?
                  Cyc-spawn-thread!
                  Cyc-end-thread!
                  apply
                  %halt
                  exit
                  system
                  command-line-arguments
                  Cyc-installation-dir
                  Cyc-default-exception-handler
                  Cyc-current-exception-handler
                  cell-get
                  set-global!
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

(define (prim-call? exp)
  (and (list? exp) (prim? (car exp))))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; define : exp -> boolean
(define (define? exp)
  (tagged-list? 'define exp))

(define (define-lambda? exp)
  (let ((var (cadr exp)))
    (or
      ;; Standard function
      (and (list? var) 
           (> (length var) 0)
           (symbol? (car var)))
      ;; Varargs function
      (and (pair? var)
           (symbol? (car var))))))

(define (define->lambda exp)
  (cond
    ((define-lambda? exp)
     (let ((var (caadr exp))
           (args (cdadr exp))
           (body (cddr exp)))
       `(define ,var (lambda ,args ,@body))))
    (else exp)))

; define->var : define-exp -> var
(define (define->var exp)
  (cond
    ((define-lambda? exp)
     (caadr exp))
    (else 
     (cadr exp))))

; define->exp : define-exp -> exp
(define (define->exp exp)
  (cddr exp))

(define (define-c? exp)
  (tagged-list? 'define-c exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!->var : set!-exp -> var
(define (set!->var exp)
  (cadr exp))

; set!->exp : set!-exp -> exp
(define (set!->exp exp)
  (caddr exp))

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


;; Macro expansion

;TODO: modify this whole section to use macros:get-env instead of *defined-macros*. macro:get-env becomes the mac-env. any new scopes need to extend that env, and an env parameter needs to be added to (expand). any macros defined with define-syntax use that env as their mac-env (how to store that)?
; expand : exp -> exp
(define (expand exp env)
  ;(trace:error `(expand ,exp))
  (cond
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((quote? exp)      exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,@(expand-body '() (lambda->exp exp) env)
                          ;,@(map 
                          ;  ;; TODO: use extend env here?
                          ;  (lambda (expr) (expand expr env))
                          ;  (lambda->exp exp))
                         ))
    ((define? exp)     (if (define-lambda? exp)
                           (expand (define->lambda exp) env)
                          `(define ,(expand (define->var exp) env)
                                ,@(expand (define->exp exp) env))))
    ((set!? exp)       `(set! ,(expand (set!->var exp) env)
                              ,(expand (set!->exp exp) env)))
    ((if? exp)         `(if ,(expand (if->condition exp) env)
                            ,(expand (if->then exp) env)
                            ,(if (if-else? exp)
                                 (expand (if->else exp) env)
                                 ;; Insert default value for missing else clause
                                 ;; FUTURE: append the empty (unprinted) value
                                 ;; instead of #f
                                 #f)))
    ((app? exp)
     (cond
;; TODO: could check for a define-syntax here and load into memory
;; if found. would then want to continue expanding. may need to 
;; return some value such as #t or nil as a placeholder, since the
;; define-syntax form would not be carried forward in the compiled code
     ((define-syntax? exp) ;; TODO: not good enough, should do error checking, and make sure list is big enough for cadr
      ;(trace:info `(define-syntax ,exp))
      (let* ((name (cadr exp))
             (trans (caddr exp))
             (body (cadr trans)))
        (cond
         ((tagged-list? 'syntax-rules trans) ;; TODO: what if syntax-rules is renamed?
          (expand
            `(define-syntax ,name ,(expand trans env))
            env))
         (else
        (set! *defined-macros* (cons (cons name body) *defined-macros*))
        ;; Keep track of macros added during compilation.
        ;; Previous list should eventually go away once macros are
        ;; moved from that static list to libraries
        (macro:add! name body)
        (env:define-variable! name (list 'macro body) env)
        ;; Keep as a 'define' form so available at runtime
        ;; TODO: may run into issues with expanding now, before some
        ;; of the macros are defined. may need to make a special pass
        ;; to do loading or expansion of macro bodies
        ;; TODO: would it be better to use *define-macros* directly instead
        ;; of trying to define it here? that might help prevent issues where
        ;; an expand is called here before all macros are defined yet
        ;;  - no, we need to do this here so code is carried though all transforms
        ;;    (alpha, cps, closure, etc). otherwise code has to be interpreted during expansion
        ;;
        `(define ,name ,(expand body env))))))
     ((define-c? exp) exp)
     ((symbol? (car exp))
      (let ((val (env:lookup (car exp) env #f)))
        (if (tagged-list? 'macro val)
          (expand ; Could expand into another macro
            (macro:expand exp val env)
            env)
          (map
            (lambda (expr) (expand expr env))
            exp))))

     (else
       (map 
        (lambda (expr) (expand expr env))
        exp))))
    (else
      (error "unknown exp: " exp))))

;; Nicer interface to expand-body
(define (expand-lambda-body exp env)
  (expand-body '() exp env))

;; Helper to expand a lambda body, so we can splice in any begin's
(define (expand-body result exp env)
  (cond
   ((null? exp) (reverse result))
   ;; Splice in begin contents and keep expanding body
   ((begin? (car exp))
    (let* ((expr (car exp))
           (begin-exprs (begin->exps expr)))
    (expand-body
     result
     (append begin-exprs (cdr exp))
     env)))
   (else
    (let ((macro #f))
      (when (and (app? (car exp))
                 (symbol? (caar exp)))
        (set! macro (env:lookup (caar exp) env #f)))
      (if (tagged-list? 'macro macro)
          ;; Expand macro here so we can catch begins in the expanded code,
          ;; including nested begins
          (let ((expanded (macro:expand (car exp) macro env)))
            ;; Call with expanded macro in case we need to expand again
            (expand-body
              result
              (cons expanded (cdr exp))
              env))
          ;; No macro, use main expand function to process
          (expand-body
           (cons 
             (expand (car exp) env)
             result)
           (cdr exp)
           env))))))

;; Top-level analysis

; Separate top-level defines (globals) from other expressions
;
; This function extracts out non-define statements, and adds them to 
; a "main" after the defines.
;
(define (isolate-globals exp program? lib-name)
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
               `((begin 0 ,@(reverse exprs)))
             )
             (else
               ;; This is a library, keep inits in their own function
               `((define ,(lib:name->symbol lib-name)
                  (lambda () 0 ,@(reverse exprs))))))
           (macro:get-env))))
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
               ;; global may still be init'd to nil if the order is incorrect in the "top level"
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
            (set! globals (cons (define->var e) globals))))
      exp)
    globals))

;; Remove global variables that are not used by the rest of the program.
;; Many improvements can be made, including:
;;
;; TODO: remove unused locals
(define (filter-unused-variables asts lib-exports)
  (define (do-filter code)
    (let ((all-fv (apply      ;; More efficient way to do this?
                    append    ;; Could use delete-duplicates
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
  (define bound-only? 
    (and (not (null? opts))
         (car opts)))

  (define (search exp)
    (cond
      ; Core forms:
      ((const? exp)    '())
      ((prim? exp)     '())    
      ((quote? exp)    '())    
      ((ref? exp)      (if bound-only? '() (list exp)))
      ((lambda? exp)   
        (difference (reduce union (map search (lambda->exp exp)) '())
                    (lambda-formals->list exp)))
      ((if? exp)       (union (search (if->condition exp))
                              (union (search (if->then exp))
                                     (search (if->else exp)))))
      ((define? exp)     (union (list (define->var exp))
                              (search (define->exp exp))))
      ((define-c? exp) (list (define->var exp)))
      ((set!? exp)     (union (list (set!->var exp)) 
                              (search (set!->exp exp))))
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
    
    ; Sugar:
    ((let? exp)
     (map analyze-mutable-variables (map cadr (let->bindings exp)))
     (map analyze-mutable-variables (let->exp exp))
     (void))
    ((letrec? exp)
     (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
     (map analyze-mutable-variables (letrec->exp exp))
     (void))
    ((begin? exp)
     (map analyze-mutable-variables (begin->exps exp))
     (void))
    
    ; Application:
    ((app? exp)
     (map analyze-mutable-variables exp)
     (void))
    (else
     (error "unknown expression type: " exp))))


; wrap-mutables : exp -> exp
(define (wrap-mutables exp globals)
  
  (define (wrap-mutable-formals formals body-exp)
    (if (not (pair? formals))
        body-exp
        (if (is-mutable? (car formals))
            `((lambda (,(car formals))
                ,(wrap-mutable-formals (cdr formals) body-exp))
              (cell ,(car formals)))
            (wrap-mutable-formals (cdr formals) body-exp))))
  
  (cond
    ; Core forms:
    ((const? exp)    exp)
    ((ref? exp)      (if (and (not (member exp globals))
                              (is-mutable? exp))
                         `(cell-get ,exp)
                         exp))
    ((prim? exp)     exp)
    ((quote? exp)    exp)
    ((lambda? exp)   `(lambda ,(lambda->formals exp)
                        ,(wrap-mutable-formals (lambda-formals->list exp)
                                               (wrap-mutables (car (lambda->exp exp)) globals)))) ;; Assume single expr in lambda body, since after CPS phase
    ((set!? exp)     `(,(if (member (set!->var exp) globals)
                            'set-global!
                            'set-cell!) 
                        ,(set!->var exp) 
                        ,(wrap-mutables (set!->exp exp) globals)))
    ((if? exp)       `(if ,(wrap-mutables (if->condition exp) globals)
                          ,(wrap-mutables (if->then exp) globals)
                          ,(wrap-mutables (if->else exp) globals)))
    
    ; Application:
    ((app? exp)      (map (lambda (e) (wrap-mutables e globals)) exp))
    (else            (error "unknown expression type: " exp))))

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
;(write `(DEBUG convert ,ast))
;(write (newline))
    (cond
      ((const? ast) ast)
      ((quote? ast) ast)
      ((ref? ast)
       (let ((renamed (assoc ast renamed)))
         (cond
          (renamed 
            (cdr renamed))
          (else ast))))
      ((define? ast)
       ;; Only internal defines at this point, of form: (define ident value)
       `(set! ,@(map (lambda (a) (convert a renamed)) (cdr ast))))
      ((set!? ast)
         ;; Without define, we have no way of knowing if this was a
         ;; define or a set prior to this phase. But no big deal, since
         ;; the set will still work in either case, so no need to check
         `(set! ,@(map (lambda (a) (convert a renamed)) (cdr ast))))
      ((if? ast)
       ;; Add a failsafe here in case macro expansion added more
       ;; incomplete if expressions.
       ;; FUTURE: append the empty (unprinted) value instead of #f
       (let ((new-ast (if (if-else? ast)
                          `(if ,@(map (lambda (a) (convert a renamed)) (cdr ast)))
                          (convert (append ast '(#f)) renamed))))
         ;; Optimization - convert (if (not a) b c) into (if a c b)
         (cond
          ((and (app? (if->condition new-ast))
                (equal? 'not (app->fun (if->condition new-ast))))
           `(if ,@(app->args (if->condition new-ast))
                ,(if->else new-ast)
                ,(if->then new-ast)))
          (else
            new-ast))))
      ((prim-call? ast)
       (let ((converted
               (cons (car ast) 
                     (map (lambda (a) (convert a renamed))
                          (cdr ast)))))
         (if (precompute-prim-app? converted)
           (eval converted) ;; OK, evaluate at compile time
           converted)))
      ((lambda? ast)
       (let* ((args (lambda-formals->list ast))
              (ltype (lambda-formals-type ast))
              (a-lookup (map (lambda (a) (cons a (gensym a))) args))
              (body (lambda->exp ast))
              (define-vars (find-defined-vars body))
              (defines-a-lookup (make-a-lookup define-vars))
             )
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
       (map (lambda (a) (convert a renamed)) ast))
      (else
        (error "unhandled expression: " ast))))

  (let* ((fv (difference (free-vars ast) globals))
         ;; Only find set! and lambda vars
         (bound-vars (union globals (free-vars ast #t)))
         ;; vars never bound in prog, but could be built-in
         (unbound-vars (difference fv bound-vars))
         ;; vars we know nothing about - error!
         (unknown-vars (difference unbound-vars (built-in-syms)))
         )
    (cond
     ((> (length unknown-vars) 0)
      (let ((unbound-to-return (list)))
        (if (member 'eval unknown-vars) 
            (set! unbound-to-return (cons 'eval unbound-to-return)))
        (if (or (member 'read unknown-vars) 
                (member 'read-all unknown-vars))
            (set! unbound-to-return (cons 'read unbound-to-return)))
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
                    (list (list 'lambda
                           (list k)
                           (xform k))
                          cont-ast)))))

          ((prim-call? ast)
           (cps-list (cdr ast) ; args to primitive function
                     (lambda (args)
                        (list cont-ast
                            `(,(car ast) ; op
                              ,@args)))))

          ((lambda? ast)
           (let ((k (gensym 'k))
                 (ltype (lambda-formals-type ast)))
             (list cont-ast
                   `(lambda
                      ,(list->lambda-formals
                         (cons k (cadr ast)) ; lam params
                         (if (equal? ltype 'args:varargs)
                             'args:fixed-with-varargs ;; OK? promote due to k
                             ltype))
                      ,(cps-seq (cddr ast) k)))))

;
; TODO: begin is expanded already by desugar code... better to do it here?
;          ((seq? ast)
;           (cps-seq (ast-subx ast) cont-ast))

          ((app? ast)
           (let ((fn (app->fun ast)))
             (cond
              ((lambda? fn)
                 (cps-list (app->args ast)
                           (lambda (vals)
                             (cons (list
                                     'lambda
                                     (lambda->formals fn)
                                     (cps-seq (cddr fn) ;(ast-subx fn)
                                                    cont-ast))
                                    vals))))
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
                  `(lambda (,r) ,(body r)))))))

  (define (cps-seq asts cont-ast)
    (cond ((null? asts)
           (list cont-ast #f))
          ((null? (cdr asts))
           (cps (car asts) cont-ast))
          (else
           (let ((r (gensym 'r)))
             (cps (car asts)
                  `(lambda
                     (,r)
                    ,(cps-seq (cdr asts) cont-ast)))))))

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

;; CPS optimizations

;; TODO: don't think we can assume lambda body is single expr, if we want
;;       to do optimizations such as inlining
(define (cps-optimize-01 exp)
  (define (opt-lambda exp)
    (let ((body (car (lambda->exp exp)))) ;; Single expr after CPS
      ;(trace:error `(DEBUG 
      ;  ,exp
      ;  ,body
      ;  ,(if (and (pair? body) (app? body) (lambda? (car body)))
      ;    (list (app->args body)
      ;          (lambda->formals exp))
      ;    #f)))
      (cond
        ;; Does the function just call its continuation?
        ((and (pair? body) 
              (app? body)
              (lambda? (car body))
              ;; TODO: need to check body length if we allow >1 expr in a body
              ;; TODO: not sure this is good enough for all cases
              (equal? (app->args body)
                      ;(lambda->formals (car body))
                      (lambda->formals exp)
                      ))
          (cps-optimize-01 (car body)))
        (else
          `(lambda ,(lambda->formals exp)
             ,(cps-optimize-01 (car (lambda->exp exp)))) ;; Assume single expr in lambda body, since after CPS phase
  ))))
  (cond
    ; Core forms:
    ((const? exp)    exp)
    ((ref? exp)      exp)
    ((prim? exp)     exp)
    ((quote? exp)    exp)
    ((lambda? exp)   (opt-lambda exp)) 
    ((set!? exp)     `(set!
                        ,(set!->var exp) 
                        ,(cps-optimize-01 (set!->exp exp))))
    ((if? exp)       `(if ,(cps-optimize-01 (if->condition exp))
                          ,(cps-optimize-01 (if->then exp))
                          ,(cps-optimize-01 (if->else exp))))
    ; Application:
    ((app? exp)      (map (lambda (e) (cps-optimize-01 e)) exp))
    (else            (error "CPS optimize unknown expression type: " exp))))

;; Closure-conversion.
;;
;; Closure conversion eliminates all of the free variables from every
;; lambda term.
;;
;; The code below is based on a fusion of a port of the 90-min-scc code by 
;; Marc Feeley and the closure conversion code in Matt Might's scheme->c 
;; compiler.

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else 
            (loop (cdr lst) (+ i 1))))))

(define (closure-convert exp globals)
 (define (convert exp self-var free-var-lst)
  (define (cc exp)
   (cond
    ((const? exp)        exp)
    ((quote? exp)        exp)
    ((ref? exp)
      (let ((i (pos-in-list exp free-var-lst)))
        (if i
            `(%closure-ref
              ,self-var
              ,(+ i 1))
            exp)))
    ((or
        (tagged-list? '%closure-ref exp)
        (tagged-list? '%closure exp)
        (prim-call? exp))
        `(,(car exp)
          ,@(map cc (cdr exp)))) ;; TODO: need to splice?
    ((set!? exp)  `(set! ,(set!->var exp)
                         ,(cc (set!->exp exp))))
    ((lambda? exp)
     (let* ((new-self-var (gensym 'self))
            (body  (lambda->exp exp))
            (new-free-vars 
              (difference 
                (difference (free-vars body) (lambda-formals->list exp))
                globals)))
       `(%closure
          (lambda
            ,(list->lambda-formals
               (cons new-self-var (lambda-formals->list exp))
               (lambda-formals-type exp))
            ,(convert (car body) new-self-var new-free-vars)) ;; TODO: should this be a map??? was a list in 90-min-scc.
          ,@(map (lambda (v) ;; TODO: splice here?
                    (cc v))
            new-free-vars))))
    ((if? exp)  `(if ,@(map cc (cdr exp))))
    ((cell? exp)       `(cell ,(cc (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(cc (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(cc (set-cell!->cell exp))
                                   ,(cc (set-cell!->value exp))))
    ((app? exp)
     (let ((fn (car exp))
           (args (map cc (cdr exp))))
       (if (lambda? fn)
           (let* ((body  (lambda->exp fn))
                  (new-free-vars 
                    (difference
                      (difference (free-vars body) (lambda-formals->list fn))
                      globals))
                  (new-free-vars? (> (length new-free-vars) 0)))
               (if new-free-vars?
                 ; Free vars, create a closure for them
                 (let* ((new-self-var (gensym 'self)))
                   `((%closure 
                        (lambda
                          ,(list->lambda-formals
                             (cons new-self-var (lambda-formals->list fn))
                             (lambda-formals-type fn))
                          ,(convert (car body) new-self-var new-free-vars))
                        ,@(map (lambda (v) (cc v))
                               new-free-vars))
                     ,@args))
                 ; No free vars, just create simple lambda
                 `((lambda ,(lambda->formals fn)
                           ,@(map cc body))
                   ,@args)))
           (let ((f (cc fn)))
            `((%closure-ref ,f 0)
              ,f
              ,@args)))))
    (else                
      (error "unhandled exp: " exp))))
  (cc exp))

 `(lambda ()
    ,(convert exp #f '())))

; Suitable definitions for the cell functions:
;(define (cell value) (lambda (get? new-value) 
;                       (if get? value (set! value new-value))))
;(define (set-cell! c v) (c #f v))
;(define (cell-get c) (c #t #t))

))
