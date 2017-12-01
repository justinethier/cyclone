;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; The Cyclone interpreter, based on the meta-circular evaluator from SICP 4.1:
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1
;;;;
(define-library (scheme eval)
  (import 
    (scheme cyclone util)
    (scheme cyclone libraries) ;; for handling import sets
    (scheme cyclone primitives)
    (scheme base)
    (scheme file)
    (scheme write) ;; Only used for debugging
    (scheme read))
  (export
    ;environment
    eval
    eval-from-c ; non-standard
    create-environment ; non-standard
    setup-environment ; non-standard
    ;; Dynamic import
    %import
    imported?
    %set-import-dirs!
    *defined-macros* 
    get-macros
    macro:macro?
    macro:expand
    macro:add!
    macro:cleanup
    macro:load-env!
    macro:get-env
    macro:get-defined-macros
    expand 
    expand-lambda-body
  )
  (inline
    primitive-implementation
    procedure-environment
    procedure-body
    procedure-parameters
    operands
    operator
    application?
    if-alternative
    if-consequent
    if-predicate
    lambda-body
    lambda-parameters
    definition-variable
    assignment-value
    assignment-variable
    variable?
    macro:macro?
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

(define (eval-from-c exp . _env)
  (let ((env (if (null? _env) *global-environment* (car _env))))
    (eval (wrapc exp) env)))

;; Expressions received from C code are already evaluated, but sometimes too much so.
;; Try to wrap 
(define (wrapc exp)
  (cond 
    ((application? exp)
     (cond
       ((compound-procedure? (car exp))
        (cons 
          (car exp)
          (map 
            (lambda (e)
              (if (self-evaluating? e)
                  e
                  `(quote ,e)))
            (cdr exp))))
       (else
        exp)))
    (else
      exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression handling helper functions
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((boolean? exp) #t)
        ((string? exp) #t)
        ((vector? exp) #t)
        ((bytevector? exp) #t)
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

(define (syntax? exp)
  (tagged-list? 'define-syntax exp))
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
(define (make-macro expr)
  (list macro-tag expr))
(define macro-tag 'macro)
(define (compound-macro? exp)
  (tagged-list? macro-tag exp))

;; Environments
(define (primitive-procedure? proc)
  (tagged-list? 'primitive proc))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (append
    (list 
      (list 'call/cc call/cc)
      (list 'Cyc-global-vars Cyc-global-vars)
      (list 'Cyc-get-cvar Cyc-get-cvar)
      (list 'Cyc-set-cvar! Cyc-set-cvar!)
      (list 'Cyc-cvar? Cyc-cvar?)
      (list 'Cyc-opaque? Cyc-opaque?)
      (list 'Cyc-has-cycle? Cyc-has-cycle?)
      (list 'Cyc-spawn-thread! Cyc-spawn-thread!)
      (list 'Cyc-end-thread! Cyc-end-thread!)
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
      (list 'Cyc-compilation-environment Cyc-compilation-environment)
      (list 'Cyc-installation-dir Cyc-installation-dir)
      (list 'system system)
      (list 'command-line-arguments command-line-arguments)
      (list 'error error)
      (list 'cons cons)
      (list 'cell-get cell-get)
      (list 'set-global! set-global!)
      (list 'set-cell! set-cell!)
      (list 'cell cell)
      (list 'eq? eq?)
      (list 'eqv? eqv?)
      (list 'equal? equal?)
      ;(list 'Cyc-fast-assoc Cyc-fast-assoc)
      (list 'assq assq)
      (list 'assv assv)
      (list 'memq memq)
      (list 'memv memv)
      ;(list 'Cyc-fast-member Cyc-fast-member)
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
      )
    (list
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
      (list 'make-bytevector make-bytevector)
      (list 'bytevector-length bytevector-length)
      (list 'bytevector-append bytevector-append)
      (list 'Cyc-bytevector-copy Cyc-bytevector-copy)
      (list 'Cyc-utf8->string Cyc-utf8->string)
      (list 'Cyc-string->utf8 Cyc-string->utf8)
      (list 'bytevector          bytevector)
      (list 'bytevector-u8-ref   bytevector-u8-ref)
      (list 'bytevector-u8-set!  bytevector-u8-set!)
      (list 'make-vector make-vector)
      (list 'list->vector list->vector)
      (list 'vector-length vector-length)
      (list 'vector-ref vector-ref)
      (list 'vector-set! vector-set!)
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
      (list 'Cyc-macro? Cyc-macro?)
      (list 'vector? vector?)
      (list 'bytevector? bytevector?)
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
      (list 'Cyc-display Cyc-display))))

(define (primitive-procedure-names)
  (foldr 
    (lambda (x y) 
      (cons (car x) y)) 
   '() 
    primitive-procedures))

(define (primitive-procedure-objects)
  (foldr
    (lambda (proc rest) 
      (cons (list 'primitive (cadr proc)) rest))
   '()
    primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply ;apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; TODO: temporary testing
;; also, it would be nice to pass around something other than
;; symbols for primitives. could the runtime inject something into the env?
;; of course that is a problem for stuff like make_pair, that is just a
;; C macro...
;; (define (primitive-procedure? proc)
;;   (equal? proc 'cons))

(define (setup-environment . env)
  (let ((initial-env
         (if (not (null? env))
             (car env)
             (create-initial-environment))))
    (cond-expand
      (cyclone
        ;; Also include compiled variables
        (env:extend-environment
          (map (lambda (v) (car v)) (Cyc-global-vars))
          (map (lambda (v) (cdr v)) (Cyc-global-vars))
          initial-env))
      (else initial-env))))

(define (create-initial-environment)
  (env:extend-environment (primitive-procedure-names)
                          (primitive-procedure-objects)
                          env:the-empty-environment))
(define *initial-environment* (create-initial-environment))
(define *global-environment* (setup-environment (create-initial-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This step separates syntactic analysis from execution.
;; - exp => Code to analyze
;; - env => Environment used to expand macros
;;
(define (analyze exp env)
;(newline)
;(display "/* ")
;(write (list 'analyze exp))
;(display " */")
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((and (assignment? exp)
              (not (null? (cdr exp))))
         (analyze-assignment exp env))
        ((and (definition? exp)
              (not (null? (cdr exp))))
         (analyze-definition exp env))
        ((and (syntax? exp)
              (not (null? (cdr exp))))
         (analyze-syntax exp env))
        ((and (tagged-list? 'let-syntax exp)
              (not (null? (cdr exp))))
         (analyze-let-syntax exp env))
        ((and (if? exp) 
              (not (null? (cdr exp))))
         (analyze-if exp env))
        ((and (lambda? exp) 
              (not (null? (cdr exp))))
         (analyze-lambda exp env))

        ((tagged-list? 'import exp)
         (analyze-import exp env))

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

(define (analyze-let-syntax exp a-env)
  (let* ((rename-env (env:extend-environment '() '() '()))
         (expanded (expand exp a-env rename-env))
         ;(expanded (expand exp (macro:get-env) rename-env))
         (cleaned (macro:cleanup expanded rename-env))
        )
    ;; TODO: probably just create a fresh env for renames
    ;; TODO: expand, do we need to clean as well?
    ;; TODO: run results back through analyze: (analyze (expand env? rename-env?
;(write `(DEBUG ,cleaned))
;(newline)
    (analyze cleaned a-env)))

(define (analyze-syntax exp a-env)
  (let ((var (cadr exp)))
    (cond
      ((tagged-list? 'er-macro-transformer (caddr exp))
        (let ((sproc (make-macro (cadr (caddr exp))))) ;(analyze-syntax-lambda (cadr (caddr exp)) a-env)))
          (lambda (env)
            (env:define-variable! var sproc env)
            'ok)))
      (else
        ;; TODO: need to support syntax-rules, and other methods of
        ;; building a macro. maybe call into something like 
        ;; analyze-syntax-lambda instead of erroring here
        (error "macro syntax not supported yet")))))

;(define (analyze-syntax-lambda exp a-env)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence (lambda-body exp) a-env)))
;    (write `(debug ,(lambda-body exp)))
;    ;(lambda (env) 
;      (make-macro `(lambda ,vars ,@(lambda-body exp)))))

(define (analyze-import exp env)
  (lambda (env)
    ;; FUTURE: allow %import to take env?
    ;(write `(%import ,(cdr exp)))
    (apply %import (cdr exp))
    'ok))

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
             (define use-env (env:extend-environment '() '() '()))
             (if (Cyc-macro? macro-op)
               ;; Compiled macro, call directly
               (analyze (apply macro-op
                              (list (cons (car exp) (operands exp))
                                    (Cyc-er-rename use-env a-env)
                                    (Cyc-er-compare? use-env use-env))) 
                        a-env)
               ;; Interpreted macro, build expression and eval
               (let ((expr (cons macro-op 
                             (list (cons 'quote 
                                         (list (cons (car exp) 
                                                     (operands exp))))
                                   (Cyc-er-rename use-env a-env)
                                   (Cyc-er-compare? use-env use-env)))))
                 (analyze
                   (eval expr a-env) ;; Expand macro
                   a-env))))))
    (cond
      ;; compiled macro
      ((Cyc-macro? var)
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
          (env:extend-environment 
            (formals->list 
              (procedure-parameters proc))
            (pack-lambda-arguments 
               (procedure-parameters proc)
               args)
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
  
(define *append-dirs* '())
(define *prepend-dirs* '())
(define (%set-import-dirs! append-dirs prepend-dirs)
  (set! *append-dirs* append-dirs)
  (set! *prepend-dirs* prepend-dirs))

;; TODO: right now this is a hack, just get all the imports sets and call their entry point
;; function to initialize them. longer-term will need to only load the specific identifiers
;; called out in the import sets
;;
;; TODO: for some imports (prefix, maybe other stuff), can we do the renaming in the env??
(define (%import . import-sets)
  (let (;; Libraries explicitly listed in the import expression
        (explicit-lib-names 
          (map lib:import->library-name (lib:list->import-set import-sets)))
        ;; All dependent libraries
        (lib-names (lib:get-all-import-deps import-sets *append-dirs* *prepend-dirs*)))
    (for-each
      (lambda (lib-name)
        (let* ((us (lib:name->unique-string lib-name))
               (loaded? (c:lib-loaded? us)))
          (if (or (not loaded?) 
                  (member lib-name explicit-lib-names))
            (c:import-shared-obj
              (lib:import->filename lib-name ".so" *append-dirs* *prepend-dirs*)
              (string-append
                "c_" (lib:name->string lib-name) "_entry_pt_first_lambda"))
            ;(begin (write `(,lib-name ,us ,loaded? is already loaded skipping)) (newline))
           )))
      lib-names)
    (set! *global-environment* (setup-environment *initial-environment*))
    #t))

;; Is the given library loaded?
(define (imported? lis)
  (c:lib-loaded? (lib:name->unique-string (lib:list->import-set lis))))

;; Wrapper around the actual shared object import function
(define-c c:import-shared-obj
  "(void *data, int argc, closure _, object k, object fn, object entry_fnc)"
  " Cyc_import_shared_object(data, k, fn, entry_fnc); ")

(define-c c:lib-loaded?
  "(void *data, int argc, closure _, object k, object name)"
  " Cyc_check_str(data, name);
    return_closcall1(data, k, is_library_loaded(string_str(name))); ")

;; Macro section
    ;; top-level macro environment
    (define *macro:env* '())

    ;; A list of all macros defined by the program/library being compiled
    (define *macro:defined-macros* '())

    (define (macro:add! name body)
      (set! *macro:defined-macros* 
        (cons (cons name body) *macro:defined-macros*))
      #t)

    (define (macro:load-env! defined-macros base-env)
      (set! *macro:env* (env:extend-environment
                          (map car defined-macros)
                          (map (lambda (v)
                                 (list 'macro (cdr v)))
                               defined-macros)
                          base-env)))

    (define (macro:get-env) *macro:env*)

    (define (macro:get-defined-macros) *macro:defined-macros*)

    ;; Macro section
    (define (macro:macro? exp defined-macros) (assoc (car exp) defined-macros))

    (define (macro:expand exp macro mac-env rename-env)
      (let* ((use-env (env:extend-environment '() '() '()))
             (compiled-macro? (or (Cyc-macro? (Cyc-get-cvar (cadr macro)))
                                  (procedure? (cadr macro))))
             (result #f))
        ;(newline)
        ;(display "/* ")
        ;(display (list 'macro:expand exp macro compiled-macro?))
        ;(display "*/ ")

          ;; Invoke ER macro
        (set! result
          (cond
            ((not macro)
              (error "macro not found" exp))
            (compiled-macro?
              ((Cyc-get-cvar (cadr macro))
                exp
                (Cyc-er-rename use-env mac-env)
                (Cyc-er-compare? use-env rename-env)))
            (else
              (eval
                (list
                  (Cyc-get-cvar (cadr macro))
                  (list 'quote exp)
                  (Cyc-er-rename use-env mac-env)
                  (Cyc-er-compare? use-env rename-env))
                mac-env))))
;        (newline)
;        (display "/* ")
;        (display (list 'macro:expand exp macro compiled-macro?))
;        (newline)
;        (display (list result))
;        (display "*/ ")
          (macro:add-renamed-vars! use-env rename-env)
          result))

    (define (macro:add-renamed-vars! env renamed-env)
      (let ((frame (env:first-frame renamed-env)))
        (for-each
          (lambda (var val)
            (env:add-binding-to-frame! var val frame))
          (env:all-variables env)
          (env:all-values env))))

    (define (macro:cleanup expr rename-env)
      (define (clean expr bv) ;; Bound variables
;(newline)
;(display "/* macro:cleanup->clean, bv =")
;(write bv)
;(newline)
;(write expr)
;(newline)
;(display "*/ ")
         (cond 
           ((const? expr)      expr)
           ((null? expr)       expr)
           ((quote? expr)      
            (let ((atom (cadr expr)))
              ;; Clean up any renamed symbols that are quoted
              ;; TODO: good enough for quoted pairs or do
              ;;       we need to traverse those, too?
              (if (ref? atom)
                 `(quote ,(clean atom bv))
                 expr)))
           ((define-c? expr)   expr)
           ((ref? expr)        
            ;; if symbol has been renamed and is not a bound variable,
            ;; undo the rename
            (let ((val (env:lookup expr rename-env #f)))
              (if (and val (not (member expr bv)))
                  (clean val bv)
                  expr)))
           ((if-syntax? expr)
            `(if ,(clean (if->condition expr) bv)
                 ,(clean (if->then expr) bv)
                 ,(if (if-else? expr)
                      (clean (if->else expr) bv)
                      #f)))
           ((lambda? expr)
            `(lambda ,(lambda->formals expr)
               ,@(map (lambda (e) 
                        (clean e (append 
                                   (lambda-formals->list expr) 
                                   bv)))
                      (lambda->exp expr))))
           ;; At this point defines cannot be in lambda form. 
           ;; EG: (define (f x) ...)
           ((define? expr)
            (let ((bv* (cons (define->var expr) bv)))
              `(define ,(define->var expr)
                       ,@(map
                          (lambda (e) (clean e bv*)) 
                          (define->exp expr)))))
           ;; For now, assume set is not introducing a new binding
           ((set!? expr)
            `(set! ,(clean (set!->var expr) bv)
                   ,(clean (set!->exp expr) bv)))
           ((app? expr) 
            (map (lambda (e) (clean e bv)) 
                 expr))
           (else
            (error "macro cleanup unexpected expression: " expr))))
      (clean expr '()))
      
    ; TODO: get macro name, transformer
    ; TODO: let-syntax forms

;; Macro expansion

;TODO: modify this whole section to use macros:get-env instead of *defined-macros*. macro:get-env becomes the mac-env. any new scopes need to extend that env, and an env parameter needs to be added to (expand). any macros defined with define-syntax use that env as their mac-env (how to store that)?
; expand : exp -> exp

;; TODO: need a local version of each expand that receives a local env built by
;;       let-syntax forms
;;(define (expand exp env rename-env local-env)
;;(define (_expand exp env rename-env)

(define (expand exp env rename-env)
  (_expand exp env rename-env '()))

(define (_expand exp env rename-env local-env)
  (define (log e)
    (display  
      (list 'expand e 'env 
        (env:frame-variables (env:first-frame env))) 
      (current-error-port))
    (newline (current-error-port)))
  ;(log exp)
;(write `(expand ,exp))
;(newline)
  (cond
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((quote? exp)      exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,@(_expand-body '() (lambda->exp exp) env rename-env local-env)
                          ;,@(map 
                          ;  ;; TODO: use extend env here?
                          ;  (lambda (expr) (_expand expr env rename-env local-env))
                          ;  (lambda->exp exp))
                         ))
    ((define? exp)     (if (define-lambda? exp)
                           (_expand (define->lambda exp) env rename-env local-env)
                          `(define ,(_expand (define->var exp) env rename-env local-env)
                                ,@(_expand (define->exp exp) env rename-env local-env))))
    ((set!? exp)       `(set! ,(_expand (set!->var exp) env rename-env local-env)
                              ,(_expand (set!->exp exp) env rename-env local-env)))
    ((if-syntax? exp)  `(if ,(_expand (if->condition exp) env rename-env local-env)
                            ,(_expand (if->then exp) env rename-env local-env)
                            ,(if (if-else? exp)
                                 (_expand (if->else exp) env rename-env local-env)
                                 ;; Insert default value for missing else clause
                                 ;; FUTURE: append the empty (unprinted) value
                                 ;; instead of #f
                                 #f)))
    ((define-c? exp) exp)
    ((define-syntax? exp)
     ;(trace:info `(define-syntax ,exp))
     (let* ((name (cadr exp))
            (trans (caddr exp))
            (body (cadr trans)))
       (cond
        ((tagged-list? 'syntax-rules trans) ;; TODO: what if syntax-rules is renamed?
         (_expand
           `(define-syntax ,name ,(_expand trans env rename-env local-env))
           env rename-env local-env))
        (else
         ;; TODO: for now, do not let a compiled macro be re-defined.
         ;; this is a hack for performance compiling (scheme base)
         (let ((macro (env:lookup name env #f)))
          (cond
            ((and (tagged-list? 'macro macro)
                  (or (Cyc-macro? (Cyc-get-cvar (cadr macro)))
                      (procedure? (cadr macro))))
             ;(trace:info `(DEBUG compiled macro ,name do not redefine))
            )
            (else
             ;; Use this to keep track of macros for filtering unused defines
             (set! *defined-macros* (cons (cons name body) *defined-macros*))
             ;; Keep track of macros added during compilation.
             ;; TODO: why in both places?
             (macro:add! name body)
             (env:define-variable! name (list 'macro body) env)))
          ;; Keep as a 'define' form so available at runtime
          ;; TODO: may run into issues with expanding now, before some
          ;; of the macros are defined. may need to make a special pass
          ;; to do loading or expansion of macro bodies
          `(define ,name ,(_expand body env rename-env local-env)))))))
    ((let-syntax? exp)
     (let* ((body (cons 'begin (cddr exp)))
            (bindings (cadr exp))
            (bindings-as-macros
              (map 
                (lambda (b)
                  (let* ((name (car b))
                         (binding (cadr b))
                         (binding-body (cadr binding)))
                    (cons 
                      name 
                      (list 
                        'macro
                        (if (tagged-list? 'syntax-rules binding)
                                     ;; TODO: is this ok?
                                     (cadr (_expand binding env rename-env local-env))
                                     binding-body)))))
                bindings))
            (new-local-macro-env (append bindings-as-macros local-env))
           )
;(trace:error `(let-syntax ,new-local-macro-env))
       (_expand body env rename-env new-local-macro-env) ;; TODO: new-local-macro-env 
       ))
    ((app? exp)
     (cond
       ((symbol? (car exp))
        (let ((val (let ((local (assoc (car exp) local-env)))
                     (if local
                         (cdr local)
                         (env:lookup (car exp) env #f)))))
;(write `(app DEBUG ,(car exp) ,val))
;(newline)
          (cond
           ((tagged-list? 'macro val)
            (_expand ; Could expand into another macro
              (macro:expand exp val env rename-env)
              env 
              rename-env 
              local-env))
           ((Cyc-macro? val)
            (_expand ; Could expand into another macro
              (macro:expand exp (list 'macro val) env rename-env)
              env 
              rename-env 
              local-env))
           (else
            (map
              (lambda (expr) (_expand expr env rename-env local-env))
              exp)))))
       (else
         ;; TODO: note that map does not guarantee that expressions are
         ;; evaluated in order. For example, the list might be processed
         ;; in reverse order. Might be better to use a fold here and
         ;; elsewhere in (expand).
         (map 
          (lambda (expr) (_expand expr env rename-env local-env))
          exp))))
    (else
      (error "unknown exp: " exp))))

;; Nicer interface to expand-body
(define (expand-lambda-body exp env rename-env)
  (expand-body '() exp env rename-env))

;; Helper to expand a lambda body, so we can splice in any begin's
(define (expand-body result exp env rename-env)
  (_expand-body result exp env rename-env '()))

(define (_expand-body result exp env rename-env local-env)
  (define (log e)
    (display (list 'expand-body e 'env 
              (env:frame-variables (env:first-frame env))) 
             (current-error-port))
    (newline (current-error-port)))

  (if (null? exp) 
    (reverse result)
    (let ((this-exp (car exp)))
;(display (list 'expand-body this-exp) (current-error-port))
;(newline (current-error-port))
      (cond
       ((or (const? this-exp)
            (prim? this-exp)
            (ref? this-exp)
            (quote? this-exp)
            (define-c? this-exp))
;(log this-exp)
        (_expand-body (cons this-exp result) (cdr exp) env rename-env local-env))
       ((define? this-exp)
;(log this-exp)
        (_expand-body 
          (cons
            (_expand this-exp env rename-env local-env)
            result)
          (cdr exp)
          env
          rename-env
          local-env))
       ((or (define-syntax? this-exp)
            (let-syntax? this-exp)
            (lambda? this-exp)
            (set!? this-exp)
            (if? this-exp))
;(log (car this-exp))
        (_expand-body 
          (cons
            (_expand this-exp env rename-env local-env)
            result)
          (cdr exp)
          env
          rename-env 
          local-env))
       ;; Splice in begin contents and keep expanding body
       ((begin? this-exp)
        (let* ((expr this-exp)
               (begin-exprs (begin->exps expr)))
;(log (car this-exp))
        (_expand-body
         result
         (append begin-exprs (cdr exp))
         env
         rename-env
         local-env)))
       ((app? this-exp)
        (cond
          ((symbol? (caar exp))
;(log (car this-exp))
           (let ((val (let ((local (assoc (caar exp) local-env)))
                       (if local
                          (cdr local)
                          (env:lookup (caar exp) env #f)))))
;(log `(DONE WITH env:lookup ,(caar exp) ,val ,(tagged-list? 'macro val)))
            (if (tagged-list? 'macro val)
              ;; Expand macro here so we can catch begins in the expanded code,
              ;; including nested begins
              (let ((expanded (macro:expand this-exp val env rename-env)))
;(log `(DONE WITH macro:expand))
                (_expand-body
                  result
                  (cons 
                    expanded ;(macro:expand this-exp val env)
                    (cdr exp))
                  env
                  rename-env
                  local-env))
              ;; No macro, use main expand function to process
              (_expand-body
               (cons 
                 (map
                  (lambda (expr) (_expand expr env rename-env local-env))
                  this-exp)
                 result)
               (cdr exp)
               env
               rename-env
               local-env))))
          (else
;(log 'app)
           (_expand-body
             (cons 
               (map
                (lambda (expr) (_expand expr env rename-env local-env))
                this-exp)
               result)
             (cdr exp)
             env
             rename-env
             local-env))))
       (else
        (error "unknown exp: " this-exp))))))

;; Container for built-in macros
(define (get-macros) *defined-macros*)
(define *defined-macros* (list))

(define (begin->exps exp)
  (cdr exp))

  ))
