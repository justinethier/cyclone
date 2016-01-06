;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; The Cyclone interpreter, based on the meta-circular evaluator from SICP 4.1:
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1
;;
(define-library (scheme eval)
  (import 
    (scheme cyclone util)
    ;(scheme cyclone libraries) ;; for handling import sets
    (scheme base)
    (scheme file)
    ;(scheme write) ;; Only used for debugging
    (scheme read))
  (export
    ;environment
    eval
    create-environment ; non-standard
    setup-environment ; non-standard
  )
  (begin

;; From r7rs:
;; This procedure returns a specifier for the environment that
;; results by starting with an empty environment and then
;; importing each list, considered as an import set, into it.
;; (See section 5.6 for a description of import sets.) The
;; bindings of the environment represented by the specifier
;; are immutable, as is the environment itself.
;;
;; Ideally, want this to allow creating new env's on top of global-env.
;(define (environment i . is)
;  (let ((imports (cons i is)))
;    'TODO
;  ))

;; Create a new environment on top of the default one.
;; - vars => Identifiers in the new environment
;; - vals => List of each value assigned to each identifier
(define (create-environment vars vals)
  ;(write `(DEBUG vars ,vars))
  ;(write `(DEBUG vals ,vals))
  (env:extend-environment vars vals *global-environment*)) ;; TODO: setup?

(define (eval exp . env)
  (if (null? env)
      ((analyze exp *global-environment*) *global-environment*)
      ((analyze exp (car env)) (car env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression handling helper functions
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((boolean? exp) #t)
        ((string? exp) #t)
        ((vector? exp) #t)
        ((char? exp) #t)
        ((port? exp) #t)
        ((eof-object? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? 'quote exp))

(define (assignment? exp)
  (tagged-list? 'set! exp))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? 'define exp))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) ;; TODO: add (not) support
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;(define (no-operands? ops) (null? ops))
;(define (first-operand ops) (car ops))
;(define (rest-operands ops) (cdr ops))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator data structures

(define procedure-tag 'procedure)
(define (make-procedure parameters body env)
  (list procedure-tag parameters body env))
(define (compound-procedure? p)
  (tagged-list? procedure-tag p))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Evaluated macros
(define macro-tag 'macro)
(define (compound-macro? exp)
  (tagged-list? macro-tag exp))

;; Environments
(define (primitive-procedure? proc)
  (tagged-list? 'primitive proc))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list 
      (list 'call/cc call/cc)
      (list 'call-with-values call-with-values)
      (list 'Cyc-global-vars Cyc-global-vars)
      (list 'Cyc-get-cvar Cyc-get-cvar)
      (list 'Cyc-set-cvar! Cyc-set-cvar!)
      (list 'Cyc-cvar? Cyc-cvar?)
      (list 'Cyc-has-cycle? Cyc-has-cycle?)
      (list 'Cyc-spawn-thread! Cyc-spawn-thread!)
      (list 'Cyc-end-thread! Cyc-end-thread!)
      (list 'thread-sleep! thread-sleep!)
      (list 'Cyc-default-exception-handler Cyc-default-exception-handler)
      (list 'Cyc-current-exception-handler Cyc-current-exception-handler)
      (list '+ +)
      (list '- -)
      (list '* *)
      (list '/ /)
      (list '= =)
      (list '> >)
      (list '< <)
      (list '>= >=)
      (list '<= <=)
      (list 'apply apply)
      (list '%halt %halt)
      (list 'exit exit)
      (list 'Cyc-installation-dir Cyc-installation-dir)
      (list 'system system)
      (list 'command-line-arguments command-line-arguments)
      (list 'Cyc-minor-gc Cyc-minor-gc)
      (list 'error error)
      (list 'cons cons)
      (list 'cell-get cell-get)
      (list 'set-global! set-global!)
      (list 'set-cell! set-cell!)
      (list 'cell cell)
      (list 'eq? eq?)
      (list 'eqv? eqv?)
      (list 'equal? equal?)
      (list 'assoc assoc)
      (list 'assq assq)
      (list 'assv assv)
      (list 'memq memq)
      (list 'memv memv)
      (list 'member member)
      (list 'length length)
      (list 'set-car! set-car!)
      (list 'set-cdr! set-cdr!)
      (list 'car car)
      (list 'cdr cdr)
      (list 'caar caar)
      (list 'cadr cadr)
      (list 'cdar cdar)
      (list 'cddr cddr)
      (list 'caaar caaar)
      (list 'caadr caadr)
      (list 'cadar cadar)
      (list 'caddr caddr)
      (list 'cdaar cdaar)
      (list 'cdadr cdadr)
      (list 'cddar cddar)
      (list 'cdddr cdddr)
      (list 'caaaar caaaar)
      (list 'caaadr caaadr)
      (list 'caadar caadar)
      (list 'caaddr caaddr)
      (list 'cadaar cadaar)
      (list 'cadadr cadadr)
      (list 'caddar caddar)
      (list 'cadddr cadddr)
      (list 'cdaaar cdaaar)
      (list 'cdaadr cdaadr)
      (list 'cdadar cdadar)
      (list 'cdaddr cdaddr)
      (list 'cddaar cddaar)
      (list 'cddadr cddadr)
      (list 'cdddar cdddar)
      (list 'cddddr cddddr)
      (list 'char->integer char->integer)
      (list 'integer->char integer->char)
      (list 'string->number string->number)
      (list 'string-cmp string-cmp)
      (list 'string-append string-append)
      (list 'list->string list->string)
      (list 'string->symbol string->symbol)
      (list 'symbol->string symbol->string)
      (list 'number->string number->string)
      (list 'string-length string-length)
      (list 'string-ref string-ref)
      (list 'string-set! string-set!)
      (list 'substring substring)
      (list 'make-vector make-vector)
      (list 'list->vector list->vector)
      (list 'vector-length vector-length)
      (list 'vector-ref vector-ref)
      (list 'vector-set! vector-set!)
      (list 'make-mutex  make-mutex)
      (list 'mutex-lock! mutex-lock!)
      (list 'mutex-unlock! mutex-unlock!)
      (list 'mutex? mutex?)
      (list 'boolean? boolean?)
      (list 'char? char?)
      (list 'eof-object? eof-object?)
      (list 'null? null?)
      (list 'number? number?)
      (list 'real? real?)
      (list 'integer? integer?)
      (list 'pair? pair?)
      (list 'port? port?)
      (list 'procedure? procedure?)
      (list 'macro? macro?)
      (list 'vector? vector?)
      (list 'string? string?)
      (list 'symbol? symbol?)
      (list 'open-input-file open-input-file)
      (list 'open-output-file open-output-file)
      (list 'close-port close-port)
      (list 'close-input-port close-input-port)
      (list 'close-output-port close-output-port)
      (list 'file-exists? file-exists?)
      (list 'delete-file delete-file)
      (list 'read-char read-char)
      (list 'peek-char peek-char)
      (list 'Cyc-read-line Cyc-read-line)
      (list 'Cyc-write-char Cyc-write-char)
      (list 'Cyc-write Cyc-write)
      (list 'Cyc-display Cyc-display)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply ;apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; TODO: temporary testing
;; also, it would be nice to pass around something other than
;; symbols for primitives. could the runtime inject something into the env?
;; of course that is a problem for stuff like make_cons, that is just a
;; C macro...
;; (define (primitive-procedure? proc)
;;   (equal? proc 'cons))

(define (setup-environment)
  (let ((initial-env
         (env:extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             env:the-empty-environment)))
    (cond-expand
      (cyclone
        ;; Also include compiled variables
        (env:extend-environment
          (map (lambda (v) (car v)) (Cyc-global-vars))
          (map (lambda (v) (cdr v)) (Cyc-global-vars))
          initial-env))
      (else initial-env))))
(define *global-environment* (setup-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This step separates syntactic analysis from execution.
;; - exp => Code to analyze
;; - env => Environment used to expand macros
;;
(define (analyze exp env)
;(newline)
;(display "/* ")
;(display (list 'analyze exp))
;(display " */")
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp env))
        ((definition? exp) (analyze-definition exp env))
        ((if? exp) (analyze-if exp env))
        ((lambda? exp) (analyze-lambda exp env))

        ;; experimenting with passing these back to eval
        ((compound-procedure? exp)
         (lambda (env) exp)) ;; TODO: good enough? update env?
        ;; END experimental code

        ((procedure? exp)
         (lambda (env) exp))
        ((application? exp) (pre-analyze-application exp env))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))
         ;(lambda () 'TODO-unknown-exp-type)))) ; JAE - this is a debug line

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (cadr exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (env:lookup-variable-value exp env)))

(define (analyze-assignment exp a-env)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp) a-env)))
    (lambda (env)
      (env:set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp a-env)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp) a-env)))
    (lambda (env)
      (env:define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp a-env)
  (let ((pproc (analyze (if-predicate exp) a-env))
        (cproc (analyze (if-consequent exp) a-env))
        (aproc (analyze (if-alternative exp) a-env)))
    (lambda (env)
      (if (pproc env)
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp a-env)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp) a-env)))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps a-env)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map (lambda (e) (analyze e a-env)) exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (pre-analyze-application exp a-env)
  ;; Notes:
  ;; look up symbol in env, and expand if it is a macro
  ;; Adds some extra overhead into eval, which is not ideal. may need to 
  ;; reduce that overhead later...

  (let* ((op (operator exp))
         (var (if (symbol? op)
                  (env:_lookup-variable-value op a-env
                    (lambda () #f)) ; Not found
                  #f))
         (expand 
           (lambda (macro-op)
             (if (macro? macro-op)
               ;; Compiled macro, call directly
               (analyze (apply macro-op
                              (list (cons (car exp) (operands exp))
                                    (Cyc-er-rename a-env)
                                    Cyc-er-compare?)) 
                        a-env)
               ;; Interpreted macro, build expression and eval
               (let ((expr (cons macro-op 
                             (list (cons 'quote 
                                         (list (cons (car exp) 
                                                     (operands exp))))
                                   (Cyc-er-rename a-env)
                                   Cyc-er-compare?))))
                 (analyze
                   (eval expr a-env) ;; Expand macro
                   a-env))))))
    (cond
      ;; compiled macro
      ((macro? var)
       (expand var))
      ;; compiled or interpreted macro in compound form
      ((compound-macro? var)
       (let ((macro (Cyc-get-cvar (cadr var))))
         (expand macro)))
      ;; standard interpreted macro
      ((compound-macro? op)
       (expand (cdr op)))
      ;; normal function
      (else
       (analyze-application exp a-env)))))

(define (analyze-application exp a-env)
  (let ((fproc (analyze (operator exp) a-env))
        (aprocs (map (lambda (o)
                       (analyze o a-env))
                     (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (env:extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        ((procedure? proc)
         (apply 
           proc 
           (map 
             (lambda (a)
               (cond
                 ;; "unwrap" objects before passing to runtime
                 ((primitive-procedure? a)
                  (primitive-implementation a))
                 (else a)))
             args)))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;(define (analyze-application exp)
;  (let ((fproc (analyze (operator exp)))
;        (aprocs (operands exp))) ; TODO: (map analyze (operands exp))))
;    (lambda (env)
;      (execute-application (fproc env)
;; TODO:                           (map (lambda (aproc) (aproc env))
;        aprocs)))) ;; TODO: temporary testing w/constants
;; TODO:                                aprocs)))))
;(define (execute-application proc args)
;  (cond ((primitive-procedure? proc)
;         (apply proc args))
;         ;(apply-primitive-procedure proc args))
;;; TODO:
;;        ;((compound-procedure? proc)
;;        ; ((procedure-body proc)
;;        ;  (env:extend-environment (procedure-parameters proc)
;;        ;                      args
;;        ;                      (procedure-environment proc))))
;        (else
;#f))) ;; TODO: this is a temporary debug line
;;         (error
;;          "Unknown procedure type -- EXECUTE-APPLICATION"
;;          proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAE - Testing, should work both with cyclone and other compilers (husk, chicken, etc)
;;       although, that may not be possible with (app) and possibly other forms. 
;(write (eval 2 *global-environment*))
;(write (eval ''(1 2) *global-environment*))
;(write (eval ''(1 . 2) *global-environment*))
;(write (eval '(if #t 'test-ok 'test-fail) *global-environment*))
;(write (eval '(if 1 'test-ok) *global-environment*))
;(write (eval '(if #f 'test-fail 'test-ok) *global-environment*))
;(write (eval '((lambda (x) (cons x 2) (cons #t x)) 1) *global-environment*))
;;(write (eval '((lambda () (cons 1 2) (cons #t #f))) *global-environment*))
;;(write (eval '(cons 1 2) *global-environment*)) ; TODO
;;(write (eval '(+ 1 2) *global-environment*)) ; TODO

;(define (loop)
;  (display (eval (read) *global-environment*))
;  (display (newline))
;  (loop))
;(loop)
  
  ))
