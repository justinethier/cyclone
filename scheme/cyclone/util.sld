;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains various utility functions.
;;;;
(define-library (scheme cyclone util)
  (import (scheme base)
          (scheme char))
  (export
    ;; Code analysis
    define-syntax?
    let-syntax?
    letrec-syntax?
    tagged-list?
    if?
    if-syntax?
    begin?
    lambda?
    pair->list 
    define-lambda? 
    define->lambda 
    formals->list
    lambda-formals->list
    lambda-varargs?
    lambda->formals
    lambda->exp 
    lambda-formals-type
    lambda-varargs-var
    pack-lambda-arguments
    if->condition 
    if->then 
    if-else? 
    if->else 
    const? 
    ref? 
    quote? 
    define-c?
    set!? 
    set!->var 
    set!->exp 
    define? 
    define->var 
    define->exp 
    app?
    ;; Environments
    env:enclosing-environment
    env:first-frame
    env:the-empty-environment
    env:make-frame
    env:frame-variables
    env:frame-values 
    env:add-binding-to-frame! 
    env:all-variables
    env:all-values
    env:extend-environment 
    env:lookup
    env:lookup-variable-value 
    env:_lookup-variable-value 
    env:set-variable-value! 
    env:define-variable! 
    ;; Syntactic closures
    make-syntactic-closure
    strip-syntactic-closures
    identifier->symbol
    identifier?
    identifier=?
    ;; ER macro supporting functions
    Cyc-er-rename
    Cyc-er-compare?
    ;; Code generation
    mangle
    mangle-global
    ;; Inlines (TBD, this may move)
    define-c-inline?
    define-c->inline-var
    ;; Immutable objects
    Cyc-set-immutable!
    ;; String functions
    string-join
    string-split
    ;; Scheme library functions
    gensym
    delete
    delete-duplicates
    flatten
    length/obj
    list-index2
    list-insert-at!
    list-prefix?
    string-replace-all
    take
    drop
    filter)
  (inline
    env:frame-values
    env:frame-variables
    env:first-frame
    env:enclosing-environment
    lambda->exp
    lambda->formals
    define->exp
    set!->exp
    set!->var
    ref?
    app?
    if->else
    if->then
    if->condition
    tagged-list?
  )
  (begin

(define (tagged-list? tag exp)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

(define (if-syntax? exp)
  (and 
    (if? exp)
    (or 
      (= (length exp) 3)
      (= (length exp) 4))))

(define (define-syntax? exp)
  (tagged-list? 'define-syntax exp))

(define (let-syntax? exp)
  (tagged-list? 'let-syntax exp))

(define (letrec-syntax? exp)
  (tagged-list? 'letrec-syntax exp))

; begin? : exp -> boolean
(define (begin? exp) 
  (tagged-list? 'begin exp))

; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

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

; const? : exp -> boolean
(define (const? exp)
  (or (integer? exp)
      (real? exp)
      (complex? exp)
      (eq? (void) exp) ;; Poor man's (void?)
      (string? exp)
      (vector? exp)
      (bytevector? exp)
      (char? exp)
      (boolean? exp)))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; quote? : exp -> boolean
(define (quote? exp)
  (tagged-list? 'quote exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!->var : set!-exp -> var
(define (set!->var exp)
  (cadr exp))

; set!->exp : set!-exp -> exp
(define (set!->exp exp)
  (caddr exp))

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

(define (define-c-inline? exp)
  (and (define-c? exp) (= (length exp) 6)))

(define (define-c->inline-var exp)
  (let ((var (define->var exp)))
    (string->symbol
      (string-append
        (symbol->string var)
        "__inline__"))))

;; Create a proper copy of an improper list
;; EG: (1 2 . 3) ==> (1 2 3)
(define (pair->list p)
  (let loop ((lst p))
    (if (not (pair? lst))
        (cons lst '())
        (cons (car lst) (loop (cdr lst))))))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (cddr exp)) ;; JAE - changed from caddr, so we can handle multiple expressions

(define (lambda-varargs-var exp)
  (if (lambda-varargs? exp)
    (if (equal? (lambda-formals-type exp) 'args:varargs)
        (lambda->formals exp) ; take symbol directly
        (car (reverse (lambda-formals->list exp)))) ; Last arg is varargs
    #f))

;(define (lambda-varargs? exp)
;  (and (lambda? exp)
;       (or (symbol? (lambda->formals exp))
;           (and (pair? (lambda->formals exp))
;                (not (list? (lambda->formals exp)))))))
; Alternate definition, works even if exp is not a lambda (IE, an AST):
(define (lambda-varargs? exp)
  (let ((type (lambda-formals-type exp)))
    (or (equal? type 'args:varargs)
        (equal? type 'args:fixed-with-varargs))))

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

;; object -> list
;; Accept only args instead of a whole lambda
(define (formals->list args)
  (cond
   ((symbol? args) (list args))
   ((list? args) args)
   (else (pair->list args))))

;; Take arguments for a lambda and pack them depending upon lambda type
(define (pack-lambda-arguments formals args)
  (cond
    ((symbol? formals)
     (list args))
    ((list? formals)
     args)
    (else
     (let ((num-req-args (length/obj formals))
           (num-args (length args)))
      (if (> num-req-args num-args)
          (error "Too few arguments supplied" formals args))
      (append 
        (take args num-req-args) ;; Required args
        (list (list-tail args num-req-args)) ;; Optional args
      )))))

(define (length/obj l)
  (let loop ((lis l)
             (len 0))
    (cond
      ((pair? lis)
       (loop (cdr lis) (+ len 1)))
      (else
       len))))

; take : list -> integer -> list
; The take function from SRFI 1
(define (take lis k)
  ;(check-arg integer? k take)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  ;(check-arg integer? k drop)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))

;; Simplified version of filter from SRFI 1
(define (filter pred lis)
  (letrec ((recur (lambda (lis)
                    (if (null? lis)
                     lis
                     (let ((head (car lis))
                           (tail (cdr lis)))
                       (if (pred head)
                           (let ((new-tail (recur tail)))
                         (if (eq? tail new-tail) lis
                             (cons head new-tail)))
                           (recur tail)))))))
    (recur lis)))

;; Based off corresponding SRFI-1 definition
(define (delete x lis)
  (filter (lambda (y) (not (equal? x y))) lis))

;; Inefficient version based off code from SRFI-1
(define (delete-duplicates lis)
  (define (recur lis) ; ((lis lis))
    (if (null? lis) lis
        (let* ((x (car lis))
               (tail (cdr lis))
               (new-tail (recur (delete x tail))))
          (if (eq? tail new-tail) lis (cons x new-tail)))))
  (recur lis))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

;; Insert obj at index k of list, increasing length of list by one.
(define (list-insert-at! lis obj k)
  (cond
   ((null? lis) (error "list-insert-at!, lis cannot be null"))
   ((and (> k 0) (null? (cdr lis)))
    (set-cdr! lis (cons obj '())))
   ((zero? k)
    (let ((old-car (car lis)))
      (set-car! lis obj)
      (set-cdr! lis (cons old-car (cdr lis)))))
   (else
    (list-insert-at! (cdr lis) obj (- k 1)))))

;; Find index of element in list, or -1 if not found
(define list-index2
  (lambda (e lst)
    (if (null? lst)
      -1
      (if (eq? (car lst) e)
        0
        (if (= (list-index2 e (cdr lst)) -1) 
          -1
          (+ 1 (list-index2 e (cdr lst))))))))

;; Replace all instances of needle within haystack.
;; Based on code from: 
;; http://stackoverflow.com/a/32320936/101258
(define (string-replace-all haystack needle replacement)    
  ;; most of the processing works on lists 
  ;; of char, not strings.
  (let ((haystack (string->list haystack))
        (needle (string->list needle))
        (replacement (string->list replacement))
        (needle-len (string-length needle)))
    (let loop ((haystack haystack) (acc '()))
      (cond ((null? haystack)
             (list->string (reverse acc)))
            ((list-prefix? haystack needle)
             (loop (list-tail haystack needle-len)
                   (reverse-append replacement acc)))
            (else
             (loop (cdr haystack) (cons (car haystack) acc)))))))

(define (reverse-append pre lis)
  (append (reverse pre) lis))

(define (list-prefix? lis prefix)
  (call/cc 
    (lambda (return)
      (for-each
        (lambda (x y)
          (if (not (equal? x y))
              (return #f)))
        lis
        prefix)
      (return #t))))
; Tests -
;(write (list-prefix? '(1 2 3 4 5) '(1 2)))
;(write (list-prefix? '(1 2 3) '(a 1 2 3 4)))
;(write (string-replace-all "The cat looks like a cat." "cat" "dog"))

; gensym-count : integer
(define gensym-count 0)

; gensym : symbol -> symbol
(define gensym (lambda params
                 (cond
                  ((null? params)
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append
                                        "$"
                                        (number->string gensym-count))))
                  (else
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append 
                                        (if (symbol? (car params))
                                            (symbol->string (car params))
                                            (car params))
                                        "$"
                                        (number->string gensym-count)))))))

;;;; Environments
;;;; TODO: longer-term, move these into their own module
(define (env:enclosing-environment env) (cdr env))
(define (env:first-frame env) (car env))
(define env:the-empty-environment '())

(define (env:make-frame variables values)
  (cons variables values))
(define (env:frame-variables frame) (car frame))
(define (env:frame-values frame) (cdr frame))
(define (env:add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (env:all-variables env)
  (flatten 
    (env:frame-variables
      (env:first-frame env))))

(define (env:all-values env)
  (flatten 
    (env:frame-values
      (env:first-frame env))))

(define (env:extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (env:make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (env:lookup-variable-value var env)
  (env:_lookup-variable-value var env 
    (lambda ()
      (error "Unbound variable" var))))

(define (env:_lookup-variable-value var env not-found)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (env:enclosing-environment env)))
            ((eq? var (car vars))
             (cond-expand
               (cyclone
                 (Cyc-get-cvar (car vals)))
               (else 
                 (car vals))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env env:the-empty-environment)
        (not-found)
        (let ((frame (env:first-frame env)))
          (scan (env:frame-variables frame)
                (env:frame-values frame)))))
  (env-loop env))

(define (env:lookup var env default-value)
  (env:_lookup-variable-value var env (lambda () default-value)))

(define (env:set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (env:enclosing-environment env)))
            ((eq? var (car vars))
             (cond-expand
               (cyclone
                 (if (Cyc-cvar? (car vals))
                   (Cyc-set-cvar! (car vals) val)
                   (set-car! vals val)))
               (else
                 (set-car! vals val))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env env:the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (env:first-frame env)))
          (scan (env:frame-variables frame)
                (env:frame-values frame)))))
  (env-loop env))

(define (env:define-variable! var val env)
  (let ((frame (env:first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (env:add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             ;; TODO: update compiled var
             ;; cond-expand
             ;;  if cvar
             ;;     set-cvar
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (env:frame-variables frame)
          (env:frame-values frame))))
;;;; END Environments


;;;; Syntactic closures
;;
;; For now, we are implementing a limited form of SC that only accepts
;; a symbol as the expression. This is good enough for explicit renaming
;; macros, but more work is needed if we ever wanted to have a stand alone
;; syntactic closures macro system.

(define-record-type <syn-clo>
  (make-sc env free-names expr)
  sc?
  (env sc-env)
  (free-names sc-free-names)
  (expr sc-expr))
(define (make-syntactic-closure env free-names expr)
  ;; TODO: what if expr is a syn closure?
  (make-sc env free-names expr))
(define (strip-syntactic-closures expr)
  ;; TODO: no, recursively traverse form and replace the sc's
  (identifier->symbol expr))
(define (identifier? expr)
  (or (symbol? expr)
      (sc? expr)))
(define (identifier->symbol id)
  (cond
    ((sc? id) (sc-expr id))
    ((symbol? id) id)
    (else
      (error "Invalid parameter to identifier->symbol" id))))
(define (identifier=? env1 id1 env2 id2)
  (let ((val1 (env:lookup (identifier->symbol id1) env1 #f))
        (val2 (env:lookup (identifier->symbol id2) env2 #f)))
    (eq? val1 val2)))

;;; Explicit renaming macros

(define (Cyc-er-rename use-env mac-env binding-lis)
  ((lambda (renames)
     (lambda (identifier)
;(Cyc-write `(ER rename ,identifier) (current-output-port))
;(Cyc-display "\n"  (current-output-port))
       ((lambda (binding-cell cell)
          (cond
           (binding-cell (cdr binding-cell))
           (cell (cdr cell))
           (else
              ((lambda (name)
                 (set! renames (cons (cons identifier name) renames))
                 name)

;; TODO: rename variables in use-env. do we need a cleanup env as well?
               (let ((val (env:lookup identifier mac-env 'not-defined)))
                 (cond
                   ((tagged-list? 'macro val)
                    (let ((renamed (gensym identifier)))
                      (env:define-variable! renamed val mac-env)
                      ;; Also update rename over here so it is available for 
                      ;; use later on by compare
                      (env:define-variable! renamed identifier use-env)
                      renamed))
                   ((eq? val 'not-defined)
                     ;; Unrenamed variable identifier
                     (let ((renamed (gensym identifier)))
                       (env:define-variable! renamed identifier use-env)
                       ;(env:define-variable! renamed val mac-env)
;(Cyc-write `(ER rename ,identifier to ,renamed) (current-output-port))
;(Cyc-display "\n"  (current-output-port))
                       renamed)
                     ;identifier ;; TESTING!
                   )
                   (else
                     identifier)))
               ; 
               ;(gensym identifier)
               ; gensym not good enough, need to also preserve ref trans.
               ; also note that an identifier can be an object, it does not
               ; just have to be a symbol. although, of course, the rest
               ; of the code needs to be able to handle identifiers in
               ; forms other than symbols, if that is done.
               ;
               ;(make-syntactic-closure mac-env '() identifier)
              ))))
        (assq identifier binding-lis)
        (assq identifier renames))
       ))
   ;; TODO: For now, do not allow renaming of special form symbols to 
   ;; prevent issues within the compiler
   '(
     (define . define)
     (define-syntax . define-syntax)
     (let-syntax . let-syntax)
     (letrec-syntax . letrec-syntax)
     (define-c . define-c)
     (if . if)
     (lambda . lambda)
     (quote . quote)
     (set! . set!)
;(... . ...) ;; TODO: DEBUG ONLY! take it out though and syntax-rules is broken
     (begin . begin) ;; TODO: just a quick-fix, not a long-term solution
    )))

(define (Cyc-er-compare? use-env renamed-env)
  ;; Keep looking up a symbol until the original non-renamed symbol is found
  (define (find-original-sym sym)
    (let ((val (env:lookup sym use-env #f)))
;(Cyc-write `(find-original-sym ,sym ,val) (current-output-port))
;(Cyc-display "\n"  (current-output-port))
      (if (not val)
          (set! val (env:lookup sym renamed-env #f)))
      (if val
          (find-original-sym val) ;; Keep going
          sym))) ;; There was no rename, so sym is not renamed
  (lambda (a b)
;(Cyc-write `(Cyc-er-compare? ,a ,b) (current-output-port))
;(Cyc-display "\n"  (current-output-port))
    (let* ((asym (find-original-sym a))
           (bsym (find-original-sym b))
           (result (eq? asym bsym)))
;(Cyc-write `(compare ,a ,b ,asym ,bsym ,result) (current-output-port))
;(Cyc-display "\n"  (current-output-port))
      result)))

;; Name-mangling.

;; We have to "mangle" Scheme identifiers into
;; C-compatible identifiers, because names like
;; foo-bar/baz are not identifiers in C.

; mangle : symbol -> string
(define (mangle symbol)
 (letrec
   ((m (lambda (chars)
      (if (null? chars)
        '()
        (if (or (and (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)))
                (char-numeric? (car chars)))
            (cons (car chars) (m (cdr chars)))
            (cons #\_ (append (integer->char-list (char->natural (car chars)))
                              (m (cdr chars))))))))
    (ident (list->string (m (string->list (symbol->string symbol))))))
   (if (member (string->symbol ident) *c-keywords*)
     (string-append "_" ident)
     ident)))

(define (mangle-global symbol)
  (string-append "__glo_" (mangle symbol)))

(define *c-keywords* 
 '(auto _Bool break case char _Complex const continue default do double else
   enum extern float for goto if _Imaginary inline int long register restrict
   return short signed sizeof static struct switch typedef union unsigned
   void volatile while
   list  ;; Not a keyword but reserved type
   ))
;; END name mangling section

;; string-join :: [string] -> Maybe (string, char) -> string
(define (string-join lst delim)
  (let ((delim* (if (char? delim) (string delim) delim)))
    (cond
      ((null? lst) 
        "")
      ((= (length lst) 1) 
        (car lst))
      (else
        (string-append 
          (car lst) 
          delim* 
          (string-join (cdr lst) delim*))))))

;; string-split :: string -> char -> [string]
;; Based on code from: https://codereview.stackexchange.com/q/75172/6414
(define (string-split str delim)
  (let ((add (lambda (current output)
               (cons (list->string (reverse current)) output))))
    (let loop ((input (string->list str))
               (output '())
               (current '()))
;(write `(DEBUG ,input ,output ,current))
      (if (null? input)
          (if (not (null? output))
              (reverse (add current output))
              '())
          (let ((char (car input))
                (input (cdr input)))
            (if (char=? char delim)
                (if (null? current)
                    (loop input output current) ;; Ignore delim by itself
                    (loop input (add current output) '()))
                (loop input output (cons char current))))))))

;; Immutable Object section

;; Internal helper function - set immutable field on a single obj
(define-c _Cyc-set-immutable!
   "(void *data, int argc, closure _, object k, object obj, object val)"
   "object result = boolean_f;
    if (is_object_type(obj)) {
      immutable(obj) = (val == boolean_f) ? 0 : 1;
      result = boolean_t;
    }
    return_closcall1(data, k, result); ")

;; Recursively update the immutable field for the given object
(define (Cyc-set-immutable! obj val)
  (_Cyc-set-immutable! obj val)
  (cond
    ((pair? obj) 
     (_Cyc-set-immutable! (car obj) val)
     (_Cyc-set-immutable! (cdr obj) val))
    ((vector? obj) (vector-for-each (lambda (o) (_Cyc-set-immutable! o val)) obj))))
;; END immutables

))
