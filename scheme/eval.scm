;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; The Cyclone interpreter, based on the meta-circular evaluator from SICP 4.1:
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1
;;
;; The code in this module is used both by the compiler and at runtime, so
;; when bootstrapping from a Scheme, keep in mind the code in this module 
;; cannot use features that are not also provided by Cyclone.
;;

(define (eval exp . env)
  (if (null? env)
      ((analyze exp) *global-environment*)
      ((analyze exp) (car env))))

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

(define (quasiquoted? exp)
  (tagged-list? 'quasiquote exp))

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

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

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

;; Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cond-expand
               (cyclone
                 (Cyc-get-cvar (car vals)))
               (else 
                 (car vals))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cond-expand
               (cyclone
                 (if (Cyc-cvar? (car vals))
                   (Cyc-set-cvar! (car vals) val)
                   (set-car! vals val)))
               (else
                 (set-car! vals val))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             ;; TODO: update compiled var
             ;; cond-expand
             ;;  if cvar
             ;;     set-cvar
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

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
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (cond-expand
      (cyclone
        ;; Also include compiled variables
        (extend-environment
          (map (lambda (v) (car v)) (Cyc-global-vars))
          (map (lambda (v) (cdr v)) (Cyc-global-vars))
          initial-env))
      (else initial-env))))
(define *global-environment* (setup-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived expressions
;; TODO: longer-term, this would be replaced by a macro system
(define (cond? exp) (tagged-list? 'cond exp))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f                              ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Improvement from section 4.1.7 - Separate syntactic analysis from execution
;;
;; TODO: need to finish this section
;; TODO: see 4.1.6  Internal Definitions
;;
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
         ((quoted? exp) (analyze-quoted exp))
         ((quasiquoted? exp) (analyze-quasiquoted exp))
         ((variable? exp) (analyze-variable exp))
         ((assignment? exp) (analyze-assignment exp))
         ((definition? exp) (analyze-definition exp))
         ((if? exp) (analyze-if exp))
         ((lambda? exp) (analyze-lambda exp))
      ;; TODO: ideally, macro system would handle these next three
        ((tagged-list? 'let exp)
         (let ((vars (map car  (cadr exp))) ;(let->bindings exp)))
               (args (map cadr (cadr exp))) ;(let->bindings exp))))
               (body (cddr exp)))
           (analyze
             (cons
               (cons 'lambda (cons vars body))
               args))))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
      ;; END derived expression processing

        ;; experimenting with passing these back to eval
        ((compound-procedure? exp)
         (lambda (env) exp)) ;; TODO: good enough? update env?
        ;; END experimental code

        ((procedure? exp)
         (lambda (env) exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))
         ;(lambda () 'TODO-unknown-exp-type)))) ; JAE - this is a debug line

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (cadr exp)))
    (lambda (env) qval)))

(define (analyze-quasiquoted exp)
  (error "quasiquote not supported yet by eval"))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env)
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
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
;;        ;  (extend-environment (procedure-parameters proc)
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
