;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module performs CPS analysis and optimizations.
;;;;

;(define-library (cps-optimizations) ;; For debugging via local unit tests
(define-library (scheme cyclone cps-optimizations)
  (import (scheme base)
          (scheme eval)
          (scheme cyclone util)
          (scheme cyclone ast)
          (scheme cyclone primitives)
          (scheme cyclone transforms)
          (srfi 69))
  (export
      closure-convert 
      pos-in-list 
      inlinable-top-level-lambda?
      optimize-cps 
      analyze-cps
      ;analyze-lambda-side-effects
      opt:contract
      opt:inline-prims
      opt:beta-expand
      adb:clear!
      adb:get
      adb:get/default
      adb:set!
      adb:get-db
      simple-lambda?
      one-instance-of-new-mutable-obj?
      ;; Analyze variables
      adb:make-var
      %adb:make-var
      adb:variable?
      adbv:global?  
      adbv:set-global!
      adbv:defined-by 
      adbv:set-defined-by!
      adbv:reassigned? 
      adbv:set-reassigned!
      adbv:assigned-value
      adbv:set-assigned-value!
      adbv:const? 
      adbv:set-const!
      adbv:const-value
      adbv:set-const-value!
      adbv:ref-count
      adbv:set-ref-count!
      adbv:ref-by
      adbv:set-ref-by!
      ;; Analyze functions
      adb:make-fnc
      %adb:make-fnc
      adb:function?
      adbf:simple adbf:set-simple!
      adbf:unused-params adbf:set-unused-params!
      adbf:side-effects adbf:set-side-effects!
  )
  (begin
    (define 
      *contract-env* 
      (let ((env (create-environment '() '())))
        (eval '(define Cyc-fast-plus +) env)
        (eval '(define Cyc-fast-sub -) env)
        (eval '(define Cyc-fast-mul *) env)
        (eval '(define Cyc-fast-div /) env)
        (eval '(define Cyc-fast-eq =) env)
        (eval '(define Cyc-fast-gt >) env)
        (eval '(define Cyc-fast-lt <) env)
        (eval '(define Cyc-fast-gte >=) env)
        (eval '(define Cyc-fast-lte <=) env)
        env))
    (define *adb* (make-hash-table))
    (define (adb:get-db) *adb*)
    (define (adb:clear!)
      (set! *adb* (make-hash-table)))
    (define (adb:get key) (hash-table-ref *adb* key))
    (define (adb:get/default key default) (hash-table-ref/default *adb* key default))
    (define (adb:set! key val) (hash-table-set! *adb* key val))
    (define-record-type <analysis-db-variable>
      (%adb:make-var 
        global defined-by 
        defines-lambda-id
        const const-value  
        ref-count ref-by
        reassigned assigned-value 
        app-fnc-count app-arg-count
        inlinable mutated-indirectly
        cont)
      adb:variable?
      (global adbv:global? adbv:set-global!)
      (defined-by adbv:defined-by adbv:set-defined-by!)
      (defines-lambda-id adbv:defines-lambda-id adbv:set-defines-lambda-id!)
      (const adbv:const? adbv:set-const!)
      (const-value adbv:const-value adbv:set-const-value!)
      (ref-count adbv:ref-count adbv:set-ref-count!)
      (ref-by adbv:ref-by adbv:set-ref-by!)
      ;; TODO: need to set reassigned flag if variable is SET, however there is at least
      ;; one exception for local define's, which are initialized to #f and then assigned
      ;; a single time via set
      (reassigned adbv:reassigned? adbv:set-reassigned!)
      (assigned-value adbv:assigned-value adbv:set-assigned-value!)
      ;; Number of times variable appears as an app-function
      (app-fnc-count adbv:app-fnc-count adbv:set-app-fnc-count!)
      ;; Number of times variable is passed as an app-argument
      (app-arg-count adbv:app-arg-count adbv:set-app-arg-count!)
      ;; Can a ref be safely inlined?
      (inlinable adbv:inlinable adbv:set-inlinable!)
      ;; Is the variable mutated indirectly? (EG: set-car! of a cdr)
      (mutated-indirectly adbv:mutated-indirectly? adbv:set-mutated-indirectly!)
      (cont adbv:cont? adbv:set-cont!)
    )

    (define (adbv-set-assigned-value-helper! sym var value)
      (define (update-lambda-atv! syms value)
;(trace:error `(update-lambda-atv! ,syms ,value))
        (cond
          ((ast:lambda? value)
           (let ((id (ast:lambda-id value)))
             (with-fnc! id (lambda (fnc)
               (adbf:set-assigned-to-var! 
                 fnc 
                 (append syms (adbf:assigned-to-var fnc)))))))
          ;; Follow references
          ((ref? value)
           (with-var! value (lambda (var)
             (if (not (member value syms))
                 (update-lambda-atv! (cons value syms) (adbv:assigned-value var))))))
          (else
            #f))
      )
      (adbv:set-assigned-value! var value)
      ;; TODO: if value is a lambda, update the lambda's var ref's
      ;; BUT, what if other vars point to var? do we need to add
      ;; them to the lambda's list as well?
      (update-lambda-atv! (list sym) value)
    )

    (define (adb:make-var)
      (%adb:make-var '? '? #f #f #f 0 '() #f #f 0 0 #t #f #f))

    (define-record-type <analysis-db-function>
      (%adb:make-fnc simple unused-params assigned-to-var side-effects)
      adb:function?
      (simple adbf:simple adbf:set-simple!)
      (unused-params adbf:unused-params adbf:set-unused-params!)
      (assigned-to-var adbf:assigned-to-var adbf:set-assigned-to-var!)
      (side-effects adbf:side-effects adbf:set-side-effects!)
      ;; TODO: top-level-define ?
    )
    (define (adb:make-fnc)
      (%adb:make-fnc '? '? '() #f))

    ;; A constant value that cannot be mutated
    ;; A variable only ever assigned to one of these could have all
    ;; instances of itself replaced with the value.
    (define (const-atomic? exp)
      (or (integer? exp)
          (real? exp)
          ;(string? exp)
          ;(vector? exp)
          ;(bytevector? exp)
          (char? exp)
          (boolean? exp)))

    ;; Helper to retrieve the Analysis DB Variable referenced
    ;; by sym (or use a default if none is found), and call
    ;; fnc with that ADBV.
    ;;
    ;; The analysis DB is updated with the variable, in case
    ;; it was not found.
    (define (with-var! sym fnc)
      (let ((var (adb:get/default sym (adb:make-var))))
        (fnc var)
        (adb:set! sym var)))

    ;; Non-mutating version, returns results of fnc
    (define (with-var sym fnc)
      (let ((var (adb:get/default sym (adb:make-var))))
        (fnc var)))

    (define (with-fnc id callback)
      (let ((fnc (adb:get/default id (adb:make-fnc))))
        (callback fnc)))

    (define (with-fnc! id callback)
      (let ((fnc (adb:get/default id (adb:make-fnc))))
        (callback fnc)
        (adb:set! id fnc)))

;; Determine if the given top-level function can be freed from CPS, due
;; to it only containing calls to code that itself can be inlined.
(define (inlinable-top-level-lambda? expr)
   (define (scan expr fail)
     (cond
       ((string? expr) (fail))
       ((bytevector? expr) (fail))
       ((const? expr) #t) ;; Good enough? what about large vectors or anything requiring alloca (strings, bytevectors, what else?)
       ((ref? expr) #t)
       ((if? expr)
        (scan (if->condition expr) fail)
        (scan (if->then expr) fail)
        (scan (if->else expr) fail))
       ((app? expr)
        (let ((fnc (car expr)))
          ;; If function needs CPS, fail right away
          (if (or (not (prim? fnc)) ;; Eventually need to handle user functions, too
                  (prim:cont? fnc) ;; Needs CPS
                  (prim:mutates? fnc) ;; This is too conservative, but basically
                                      ;; there are restrictions about optimizing
                                      ;; args to a mutator, so reject them for now
                  (prim-creates-mutable-obj? fnc) ;; Again, probably more conservative
                                                  ;; than necessary
              )
              (fail))
          ;; Otherwise, check for valid args
          (for-each
            (lambda (e)
              (scan e fail))
            (cdr expr))))
       ;; prim-app - OK only if prim does not require CPS.
       ;;            still need to check all its args
       ;; app - same as prim, only OK if function does not require CPS.
       ;;       probably safe to return #t if calling self, since if no
       ;;       CPS it will be rejected anyway
       ;;  NOTE: would not be able to detect all functions in this module immediately.
       ;;  would probably have to find some, then run this function successively to find others.
       ;;
       ;; Reject everything else - define, set, lambda
       (else (fail))))
  (cond
    ((and (define? expr)
          (lambda? (car (define->exp expr)))
          (equal? 'args:fixed (lambda-formals-type (car (define->exp expr)))))
     (call/cc
      (lambda (k)
        (let* ((define-body (car (define->exp expr)))
               (lambda-body (lambda->exp define-body))
               (fv (filter 
                     (lambda (v)
                       (not (prim? v)))
                     (free-vars expr)))
              )
;(trace:error `(JAE DEBUG ,(define->var expr) ,fv))
          (cond
            ((> (length lambda-body) 1)
             (k #f)) ;; Fail with more than one expression in lambda body,
                     ;; because CPS is required to compile that.
            ((> (length fv) 1) ;; Reject any free variables to attempt to prevent
             (k #f))           ;; cases where there is a variable that may be
                               ;; mutated outside the scope of this function.
            (else
              (scan
                (car lambda-body)
                (lambda () (k #f))) ;; Fail with #f
              (k #t))))))) ;; Scanned fine, return #t
    (else #f)))

    ;; Scan given if expression to determine if an inline is safe.
    ;; Returns #f if not, the new if expression otherwise.
    (define (inline-if:scan-and-replace expr kont)
      (define (scan expr fail)
;(trace:error `(inline-if:scan-and-replace:scan ,expr))
           (cond
             ((ast:lambda? expr) (fail))
             ((string? expr) (fail))
             ((bytevector? expr) (fail))
             ((const? expr) expr) ;; Good enough? what about large vectors or anything requiring alloca (strings, bytevectors, what else?)
             ((ref? expr) expr)
             ((if? expr)
              `(Cyc-if ,(scan (if->condition expr) fail)
                       ,(scan (if->then expr) fail)
                       ,(scan (if->else expr) fail)))
             ((app? expr)
              (let ((fnc (car expr)))
                ;; If function needs CPS, fail right away
                (cond
                 ((equal? (car expr) kont)
                  ;; Get rid of the continuation
                  (scan (cadr expr) fail))
                 ((or (not (prim? fnc))
                        (prim:cont? fnc)
                        (prim:mutates? fnc)
                        (prim-creates-mutable-obj? fnc)
                    )
                    (fail))
                 (else
                   ;; Otherwise, check for valid args
                   (cons
                     (car expr)
                     (map
                       (lambda (e)
                         (scan e fail))
                       (cdr expr)))))))
             ;; Reject everything else - define, set, lambda
             (else (fail))))
      (call/cc
        (lambda (return)
          (scan expr (lambda () (return #f))))))

    (define (analyze-find-lambdas exp lid)
      (cond
        ((ast:lambda? exp)
         (let* ((id (ast:lambda-id exp))
                (fnc (adb:get/default id (adb:make-fnc))))
           (adb:set! id fnc)
           ;; Flag continuation variable, if present
           (if (ast:lambda-has-cont exp)
               (let ((k (car (ast:lambda-args exp))))
                 (with-var! k (lambda (var)
                   (adbv:set-cont! var #t)))))
           (for-each
             (lambda (expr)
               (analyze-find-lambdas expr id))
             (ast:lambda-body exp))))
        ((const? exp) #f)
        ((quote? exp) #f)
        ((ref? exp) #f)
        ((define? exp)
         (let ((val (define->exp exp)))
           (if (ast:lambda? (car val))
               (with-var! (define->var exp) (lambda (var)
                 (adbv:set-defines-lambda-id! 
                   var (ast:lambda-id (car val)))))))
         (analyze-find-lambdas (define->exp exp) lid))
        ((set!? exp)
         (analyze-find-lambdas (set!->exp exp) lid))
        ((if? exp)
         (analyze-find-lambdas (if->condition exp) lid)
         (analyze-find-lambdas (if->then exp) lid)
         (analyze-find-lambdas (if->else exp) lid))
        ((app? exp)
         (for-each
           (lambda (e)
             (analyze-find-lambdas e lid))
           exp))
        (else
          #f)))

    ;; Mark each lambda that has side effects.
    ;; For nested lambdas, if a child has side effects also mark the parent
    (define (analyze-lambda-side-effects exp lid)
      (cond
        ((ast:lambda? exp)
         (let* ((id (ast:lambda-id exp))
                (fnc (adb:get/default id (adb:make-fnc))))
           (adb:set! id fnc)
           (for-each
             (lambda (expr)
               (analyze-lambda-side-effects expr id))
             (ast:lambda-body exp))
           ;; If id has side effects, mark parent lid, too
           (if (and (> lid -1)
                    (adbf:side-effects fnc))
               (with-fnc! lid (lambda (f)
                 (adbf:set-side-effects! f #t))))))
        ((const? exp) #f)
        ((quote? exp) #f)
        ((ref? exp) #f)
        ((define? exp)
         (analyze-lambda-side-effects (define->exp exp) lid))
        ((set!? exp)
         (with-fnc! lid (lambda (fnc)
           (adbf:set-side-effects! fnc #t)))
         (analyze-lambda-side-effects (set!->exp exp) lid))
        ((if? exp)
         (analyze-lambda-side-effects (if->condition exp) lid)
         (analyze-lambda-side-effects (if->then exp) lid)
         (analyze-lambda-side-effects (if->else exp) lid))
        ((app? exp)
         (let ((pure-ref #t))
           ;; Check if ref is pure. Note this may give wrong results
           ;; if ref's lambda has not been scanned yet. One solution is
           ;; to make 2 top-level passes of analyze-lambda-side-effects.
           (if (ref? (car exp))
               (with-var (car exp) (lambda (var)
                 (if (adbv:defines-lambda-id var)
                     (with-fnc! (adbv:defines-lambda-id var) (lambda (fnc)
                       (if (adbf:side-effects fnc)
                           (set! pure-ref #f))))))))

         ;; This lambda has side effects if it calls a mutating prim or
         ;; a function not explicitly marked as having no side effects.
         (if (or (prim:mutates? (car exp))
                 (and (ref? (car exp))
                      (not pure-ref)))
             (with-fnc! lid (lambda (fnc)
               (adbf:set-side-effects! fnc #t))))
         (for-each
           (lambda (e)
             (analyze-lambda-side-effects e lid))
           exp)))
        (else
          #f)))

;; TODO: check app for const/const-value, also (for now) reset them
;; if the variable is modified via set/define
    (define (analyze exp lid)
;(trace:error `(analyze ,lid ,exp ,(app? exp)))
      (cond
        ; Core forms:
        ((ast:lambda? exp)
         (let* ((id (ast:lambda-id exp))
                (fnc (adb:get/default id (adb:make-fnc))))
           ;; save lambda to adb
           (adb:set! id fnc)
           ;; Analyze the lambda
;(trace:error `(DEBUG-exp ,exp))
;(trace:error `(DEUBG-ast ,(ast:lambda-formals->list exp)))
           (for-each
            (lambda (arg)
              ;(let ((var (adb:get/default arg (adb:make-var))))
              (with-var! arg (lambda (var)
                (adbv:set-global! var #f)
                (adbv:set-defined-by! var id))))
            (ast:lambda-formals->list exp))
           (for-each
             (lambda (expr)
               (analyze expr id))
             (ast:lambda-body exp))))
        ((const? exp) #f)
        ((quote? exp) #f)
        ((ref? exp)
         (let ((var (adb:get/default exp (adb:make-var))))
          (adbv:set-ref-count! var (+ 1 (adbv:ref-count var)))
          (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))
         ))
        ((define? exp)
         ;(let ((var (adb:get/default (define->var exp) (adb:make-var))))
         (with-var! (define->var exp) (lambda (var)
           (adbv:set-defined-by! var lid)
           (adbv:set-ref-count! var (+ 1 (adbv:ref-count var)))
           (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))
           (adbv-set-assigned-value-helper! (define->var exp) var (define->exp exp))
           (adbv:set-const! var #f)
           (adbv:set-const-value! var #f)))
         (analyze (define->exp exp) lid))
        ((set!? exp)
         ;(let ((var (adb:get/default (set!->var exp) (adb:make-var))))
         (with-var! (set!->var exp) (lambda (var)
           (if (adbv:assigned-value var)
               (adbv:set-reassigned! var #t))
           (adbv-set-assigned-value-helper! (set!->var exp) var (set!->exp exp))
           (adbv:set-ref-count! var (+ 1 (adbv:ref-count var)))
           (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))
           (adbv:set-const! var #f)
           (adbv:set-const-value! var #f)))
         (analyze (set!->exp exp) lid))
        ((if? exp)       `(if ,(analyze (if->condition exp) lid)
                              ,(analyze (if->then exp) lid)
                              ,(analyze (if->else exp) lid)))
        
        ; Application:
        ((app? exp)
         (if (ref? (car exp))
             (with-var! (car exp) (lambda (var)
               (adbv:set-app-fnc-count! var (+ 1 (adbv:app-fnc-count var))))))
         (for-each
          (lambda (arg)
             (if (ref? arg)
                 (with-var! arg (lambda (var)
                   (adbv:set-app-arg-count! var (+ 1 (adbv:app-arg-count var)))))))
          (app->args exp))

         ;; Identify indirect mutations. That is, the result of a function call
         ;; is what is mutated
         (cond
          ((and (prim:mutates? (car exp)))
           (let ((e (cadr exp)))
            (when (ref? e)
              (with-var e (lambda (var)
                (if (adbv:assigned-value var)
                    (set! e (adbv:assigned-value var))))))
           ;(trace:error `(find-indirect-mutations ,e))
           (find-indirect-mutations e))))

         ;; TODO: if ast-lambda (car),
         ;; for each arg
         ;;  if arg is const-atomic
         ;;     mark the parameter (variable) as const and give it const-val
         ;;
         ;; obviously need to add code later on to reset const if mutated
         (cond
          ((and (ast:lambda? (car exp))
                (list? (ast:lambda-args (car exp)))) ;; For now, avoid complications with optional/extra args
           (let ((params (ast:lambda-args (car exp))))
             (for-each
              (lambda (arg)
;(trace:error `(app check arg ,arg ,(car params) ,(const-atomic? arg)))
                (with-var! (car params) (lambda (var)
                  (adbv-set-assigned-value-helper! (car params) var arg)
                  (cond
                   ((const-atomic? arg)
                    (adbv:set-const! var #t)
                    (adbv:set-const-value! var arg)))))
                ;; Walk this list, too
                (set! params (cdr params)))
              (app->args exp)))))
         (for-each
           (lambda (e)
             (analyze e lid))
           exp))
;TODO         ((app? exp)      (map (lambda (e) (wrap-mutables e globals)) exp))

        ; Nothing to analyze for these?
        ;((prim? exp)     exp)
        ;((quote? exp)    exp)
        ; Should never see vanilla lambda's in this function, only AST's
        ;((lambda? exp)
        ;; Nothing to analyze for expressions that fall into this branch
        (else
          #f)))

    (define (analyze2 exp)
      (cond
        ; Core forms:
        ((ast:lambda? exp)
         (let* ((id (ast:lambda-id exp))
                (fnc (adb:get id)))
;(trace:error `(adb:get ,id ,fnc))
           (adbf:set-simple! fnc (simple-lambda? exp))
           (for-each
             (lambda (expr)
               (analyze2 expr))
             (ast:lambda-body exp))))
        ((const? exp) #f)
        ((quote? exp) #f)
;; TODO:
;        ((ref? exp)
;         (let ((var (adb:get/default exp (adb:make-var))))
;          (adbv:set-ref-count! var (+ 1 (adbv:ref-count var)))
;          (adbv:set-ref-by! var (cons lid (adbv:ref-by var)))
;         ))
        ((define? exp)
         ;(let ((var (adb:get/default (define->var exp) (adb:make-var))))
           (analyze2 (define->exp exp)))
        ((set!? exp)
         ;(let ((var (adb:get/default (set!->var exp) (adb:make-var))))
           (analyze2 (set!->exp exp)))
        ((if? exp)       `(if ,(analyze2 (if->condition exp))
                              ,(analyze2 (if->then exp))
                              ,(analyze2 (if->else exp))))
        ; Application:
        ((app? exp)
         (for-each (lambda (e) (analyze2 e)) exp))
        (else #f)))

    (define (find-indirect-mutations exp)
      (cond
        ; Core forms:
        ;((ast:lambda? exp)
        ; (let* ((id (ast:lambda-id exp))
        ;        (fnc (adb:get id)))
        ;   (adbf:set-simple! fnc (simple-lambda? exp))
        ;   (for-each
        ;     (lambda (expr)
        ;       (analyze2 expr))
        ;     (ast:lambda-body exp))))
        ((const? exp) #f)
        ((quote? exp) #f)
        ((ref? exp)
         (with-var! exp (lambda (var)
           (adbv:set-mutated-indirectly! var #t))))
        ;((define? exp)
        ; ;(let ((var (adb:get/default (define->var exp) (adb:make-var))))
        ;   (analyze2 (define->exp exp)))
        ;((set!? exp)
        ; ;(let ((var (adb:get/default (set!->var exp) (adb:make-var))))
        ;   (analyze2 (set!->exp exp)))
        ((if? exp)       `(if ,(find-indirect-mutations (if->condition exp))
                              ,(find-indirect-mutations (if->then exp))
                              ,(find-indirect-mutations (if->else exp))))
        ; Application:
        ((app? exp)
         (for-each find-indirect-mutations (cdr exp)))
        (else #f)))

    ;; TODO: make another pass for simple lambda's
    ;can use similar logic to cps-optimize-01:
    ;- body is a lambda app
    ;- no lambda args are referenced in the body of that lambda app
    ;  (ref-by is empty or the defining lid)
    ;
    ; Need to check analysis DB against CPS generated and make sure
    ; things like ref-by make sense (ref by seems like its only -1 right now??)
    ;; Does ref-by list contains references to lambdas other than owner?
    ;; int -> ast-variable -> boolean
    (define (nonlocal-ref? owner-id adb-var)
      (define (loop ref-by-ids)
        (cond
          ((null? ref-by-ids) #f)
          ((not (pair? ref-by-ids)) #f)
          (else
            (let ((ref (car ref-by-ids)))
              (if (and (number? ref) (not (= owner-id ref)))
                  #t ;; Another lambda uses this variable
                  (loop (cdr ref-by-ids)))))))
      (loop (adbv:ref-by adb-var)))
      
    ;; int -> [symbol] -> boolean
    (define (any-nonlocal-refs? owner-id vars)
      (call/cc 
        (lambda (return)
          (for-each
            (lambda (var)
              (if (nonlocal-ref? owner-id (adb:get var))
                  (return #t)))
            vars)
          (return #f))))

    ;; ast-function -> boolean
    (define (simple-lambda? ast)
      (let ((body (ast:lambda-body ast))
            (formals (ast:lambda-formals->list ast))
            (id (ast:lambda-id ast)))
        (if (pair? body)
            (set! body (car body)))
;(trace:error `(simple-lambda? ,id ,formals 
;,(and (pair? body)
;     (app? body)
;     (ast:lambda? (car body)))
;,(length formals)
;;,body
;))
        (and (pair? body)
             (app? body)
             (ast:lambda? (car body))
             (> (length formals) 0)
             (equal? (app->args body)
                     formals)
             (not (any-nonlocal-refs? id formals))
    )))

    ;; Perform contraction phase of CPS optimizations
    (define (opt:contract exp)
      (cond
        ; Core forms:
        ((ast:lambda? exp)
         (let* ((id (ast:lambda-id exp))
                (fnc (adb:get id)))
           (if (adbf:simple fnc)
               (opt:contract (caar (ast:lambda-body exp))) ;; Optimize-out the lambda
               (ast:%make-lambda
                 (ast:lambda-id exp)
                 (ast:lambda-args exp)
                 (opt:contract (ast:lambda-body exp))
                 (ast:lambda-has-cont exp)))))
        ((const? exp) exp)
        ((ref? exp) 
         (let ((var (adb:get/default exp #f)))
           (if (and var (adbv:const? var))
               (adbv:const-value var)
               exp)))
        ((prim? exp) exp)
        ((quote? exp) exp)
        ((define? exp)
         `(define ,(opt:contract (define->var exp))
                  ,@(opt:contract (define->exp exp))))
        ((set!? exp)
         `(set! ,(opt:contract (set!->var exp))
                ,(opt:contract (set!->exp exp))))
        ((if? exp)
         (cond
          ((not (if->condition exp))
           (opt:contract (if->else exp)))
          (else 
            `(if ,(opt:contract (if->condition exp))
                 ,(opt:contract (if->then exp))
                 ,(opt:contract (if->else exp))))))
        ; Application:
        ((app? exp)
         ;; Beta expansion of functions only called once, from CWC
         (if (beta-expand/called-once? exp)
             (set! exp (beta-expand-app exp)))
         ;; END

         (let* ((fnc (opt:contract (car exp))))
           (cond
            ((and (ast:lambda? fnc)
                  (list? (ast:lambda-args fnc)) ;; Avoid optional/extra args
                  (= (length (ast:lambda-args fnc))
                     (length (app->args exp))))
             (let ((new-params '())
                   (new-args '())
                   (args (cdr exp)))
               (for-each
                 (lambda (param)
                   (let ((var (adb:get/default param #f)))
                     (cond
                      ((and var (adbv:const? var))
                       #f)
                      (else
                       ;; Collect the params/args not optimized-out
                       (set! new-args (cons (car args) new-args))
                       (set! new-params (cons param new-params))))
                     (set! args (cdr args))))
                 (ast:lambda-args fnc))
;(trace:e  rror `(DEBUG contract args ,(app->args exp) 
;                                new-args ,new-args
;                                params ,(ast:lambda-args fnc) 
;                                new-params ,new-params))
               (cons
                 (ast:%make-lambda
                   (ast:lambda-id fnc)
                   (reverse new-params)
                   (ast:lambda-body fnc) 
                   (ast:lambda-has-cont fnc))
                 (map 
                   opt:contract
                     (reverse new-args)))))
            (else
             (let ((result
                     (cons 
                       fnc
                       (map (lambda (e) (opt:contract e)) (cdr exp)))))
               (if (and (prim-call? exp)
                        (precompute-prim-app? result))
                   (with-handler
                      (lambda (err) result)
                      (eval result *contract-env*))
                   result))
             ))))
        (else 
          (error "CPS optimize [1] - Unknown expression" exp))))

    ;; Inline primtives
    ;; Uses analysis DB, so must be executed after analysis phase
    ;;
    ;; TBD: better to enhance CPS conversion to do this??
    (define (opt:inline-prims exp . refs*)
      (let ((refs (if (null? refs*)
                      (make-hash-table)
                      (car refs*))))
;(trace:error `(opt:inline-prims ,exp))
        (cond
          ((ref? exp) 
           ;; Replace lambda variables, if necessary
           (let ((key (hash-table-ref/default refs exp #f)))
             (if key
                 (opt:inline-prims key refs)
                 exp)))
          ((ast:lambda? exp)
           (ast:%make-lambda
            (ast:lambda-id exp)
            (ast:lambda-args exp)
            (map (lambda (b) (opt:inline-prims b refs)) (ast:lambda-body exp))
            (ast:lambda-has-cont exp)))
          ((const? exp) exp)
          ((quote? exp) exp)
          ((define? exp)
           `(define ,(define->var exp)
                    ,@(opt:inline-prims (define->exp exp) refs))) ;; TODO: map????
          ((set!? exp)
           `(set! ,(set!->var exp)
                  ,(opt:inline-prims (set!->exp exp) refs)))
          ((if? exp)       
           (cond
            ((not (if->condition exp))
             (opt:inline-prims (if->else exp) refs)) ;; Always false, so replace with else
            ((const? (if->condition exp))
             (opt:inline-prims (if->then exp) refs)) ;; Always true, replace with then
            (else
              `(if ,(opt:inline-prims (if->condition exp) refs)
                   ,(opt:inline-prims (if->then exp) refs)
                   ,(opt:inline-prims (if->else exp) refs)))))
          ; Application:
          ((app? exp)
;(trace:error `(app? ,exp ,(ast:lambda? (car exp))
;              ,(length (cdr exp))
;              ,(length (ast:lambda-formals->list (car exp)))
;              ,(all-prim-calls? (cdr exp))))
           (cond
            ((and (ast:lambda? (car exp))
                  ;; TODO: check for more than one arg??
                  (equal? (length (cdr exp))
                          (length (ast:lambda-formals->list (car exp))))
                  (or
                ;; This "and" is not for primitives, but rather checking 
                ;; for constants to optimize out. This just happens to be 
                ;; a convenient place since the optimization is the same.
                (and
                  ;; Check each parameter
                  (every
                    (lambda (param)
                      (with-var param (lambda (var)
                        (and 
                          ;; At least for now, do not replace if referenced by multiple functions
                          (<= (length (adbv:ref-by var)) 1)
                          ;; Need to keep variable because it is mutated
                          (not (adbv:reassigned? var))
                    ))))
                    (ast:lambda-formals->list (car exp)))
                  ;; Args are all constants
                  (every
                    (lambda (arg)
                      (and
                        arg ;; #f is a special value for init, so do not optimize it for now
                        (or (const? arg)
                            (quote? arg))))
                    (cdr exp))
                )
                ;; Check for primitive calls that can be optimized out
                (and
                  #;(begin
(trace:error `(DEBUG
               ,exp
                  ,(every
                    (lambda (param)
                      (with-var param (lambda (var)
;(trace:error `(DEBUG ,param ,(adbv:ref-by var)))
                        (and 
                          ;; If param is never referenced, then prim is being
                          ;; called for side effects, possibly on a global
                          (not (null? (adbv:ref-by var)))
                          ;; Need to keep variable because it is mutated
                          (not (adbv:reassigned? var))
                    ))))
                    (ast:lambda-formals->list (car exp)))
                  ;; Check all args are valid primitives that can be inlined
                  ,(every
                    (lambda (arg)
                      (and (prim-call? arg)
                           (not (prim:cont? (car arg)))))
                    (cdr exp))
                  ;; Disallow primitives that allocate a new obj,
                  ;; because if the object is mutated all copies
                  ;; must be modified. 
                  ,(one-instance-of-new-mutable-obj?
                    (cdr exp)
                    (ast:lambda-formals->list (car exp)))
                  ,(inline-prim-call? 
                    (ast:lambda-body (car exp))
                    (prim-calls->arg-variables (cdr exp))
                    (ast:lambda-formals->list (car exp)))))
                    #t)
                  ;; Double-check parameter can be optimized-out
                  (every
                    (lambda (param)
                      (with-var param (lambda (var)
;(trace:error `(DEBUG ,param ,(adbv:ref-by var)))
                        (and 
                          ;; If param is never referenced, then prim is being
                          ;; called for side effects, possibly on a global
                          (not (null? (adbv:ref-by var)))
                          ;; Need to keep variable because it is mutated
                          (not (adbv:reassigned? var))
                    ))))
                    (ast:lambda-formals->list (car exp)))
                  ;; Check all args are valid primitives that can be inlined
                  (every
                    (lambda (arg)
                      (and (prim-call? arg)
                           (not (prim:cont? (car arg)))))
                    (cdr exp))
                  ;; Disallow primitives that allocate a new obj,
                  ;; because if the object is mutated all copies
                  ;; must be modified. 
                  (one-instance-of-new-mutable-obj?
                    (cdr exp)
                    (ast:lambda-formals->list (car exp)))
                  (or
                    ; Issue #172 - Cannot assume that just because a primitive 
                    ; deals with immutable objects that it is safe to inline.
                    ; A (set!) could still mutate variables the primitive is
                    ; using, causing invalid behavior.
                    ;(prim-calls-inlinable? (cdr exp))

                    ;; Testing - every arg only used once
                    ;(and
                    ;  (every
                    ;    (lambda (param)
                    ;      (with-var param (lambda (var)
                    ;        (equal? 1 (adbv:ref-count var)))))
                    ;    (ast:lambda-formals->list (car exp)))
                    ;  ;; Do not inline if prim mutates, to avoid cases where
                    ;  ;; a var may be modified out-of-order. May be possible
                    ;  ;; to be more intelligent about this in the future.
                    ;  (every
                    ;   (lambda (arg)
                    ;     (and (prim-call? arg)
                    ;          (not (equal? 'cell (car arg)))
                    ;          (not (prim:mutates? (car arg)))))
                    ;   (cdr exp)))

                    (inline-prim-call? 
                      (ast:lambda-body (car exp))
                      (prim-calls->arg-variables (cdr exp))
                      (ast:lambda-formals->list (car exp))))))
             )
             (let ((args (cdr exp)))
               (for-each
                (lambda (param)
                  (hash-table-set! refs param (car args))
                  (set! args (cdr args)))
                (ast:lambda-formals->list (car exp))))
             (opt:inline-prims (car (ast:lambda-body (car exp))) refs))
            ;; Issue #201 - Attempt to identify case where an if can be inlined
            ((and #f ;; TODO: Disabling for now, see issue for more info
                  (= (length exp) 2)
                  (ast:lambda? (car exp))
                  (ast:lambda? (cadr exp))
                  (ast:lambda-has-cont (car exp))
                  (= 1 (length (ast:lambda-formals->list (car exp))))
                  (= 1 (length (ast:lambda-formals->list (cadr exp))))
                  (if? (car (ast:lambda-body (car exp))))
;; TODO: think we can get rid of this simplification now
                  ;; Simplification, for now only allow then/else that call a cont
                  ;; immediately, to prevent having to scan/rewrite those expressions
                  (let ((if-exp (car (ast:lambda-body (car exp))))
                        (kont (car (ast:lambda-formals->list (car exp)))))
                    (and 
                      (app? (if->then if-exp))
                      (app? (if->else if-exp))
                      ;(equal? kont (car (if->then if-exp)))
                      ;(equal? kont (car (if->else if-exp)))
                      ))
                  ;;
                  (not
                    (with-fnc (ast:lambda-id (car exp)) (lambda (fnc)
                      (adbf:side-effects fnc))))
                  )
;(trace:error `(DEBUG2 ,exp))
             (let* ((new-exp (car (ast:lambda-body (cadr exp))))
                    (old-if (car (ast:lambda-body (car exp))))
                    (old-k (car (ast:lambda-formals->list (car exp))))
                    (old-arg (car (ast:lambda-formals->list (cadr exp))))
; TODO: what about nested if's? may need another pass above to make sure
;; the if is simple enough to inline
;TODO: can logic from inlinable-top-level-lambda? be repurposed to
;scan old-if to make sure everything is inlinable???
                    (new-if 
                      (inline-if:scan-and-replace
                        `(Cyc-if ,(if->condition old-if)
                                 ,(if->then old-if)
                                 ,(if->else old-if))
                        old-k))
                    )
               #;(trace:error `(DEBUG if inline candidate 
                               ,exp
                               old-k:
                               ,old-k
                               old-arg:
                               ,old-arg
                               new-if:
                               ,new-if
                               new-exp:
                               ,new-exp
                               ))

               (cond
                (new-if
                  (hash-table-set! refs old-arg new-if)
                  (opt:inline-prims new-exp refs))
                (else
                  ;; Could not inline
                  (map (lambda (e) (opt:inline-prims e refs)) exp)))
            )) ;;
            (else
              (map (lambda (e) (opt:inline-prims e refs)) exp))))
          (else 
            (error `(Unexpected expression passed to opt:inline-prims ,exp))))))

    ;; Do all the expressions contain prim calls?
    (define (all-prim-calls? exps)
      (cond
        ((null? exps) #t)
        ((prim-call? (car exps))
         (all-prim-calls? (cdr exps)))
        (else #f)))

    ;; Find all variables passed to all prim calls
    (define (prim-calls->arg-variables exps)
      (apply
        append
        (map
          (lambda (exp)
            (cond
              ((pair? exp)
               (filter symbol? (cdr exp)))
              (else '())))
          exps)))

    ;; Does the given primitive return a new instance of an object that
    ;; can be mutated?
    ;;
    ;; TODO: strings are a problem because there are
    ;; a lot of primitives that allocate them fresh!
    (define (prim-creates-mutable-obj? prim)
      (member 
        prim
        '(
          apply ;; ??
          cons 
          make-vector 
          make-bytevector
          bytevector 
          bytevector-append 
          bytevector-copy
          string->utf8 
          number->string 
          symbol->string 
          list->string 
          utf8->string
          read-line
          string-append 
          string 
          substring 
          Cyc-installation-dir 
          Cyc-compilation-environment
          Cyc-bytevector-copy
          Cyc-utf8->string
          Cyc-string->utf8
          list->vector
          )))

    (define (prim-calls-inlinable? prim-calls)
      (every
        (lambda (prim-call)
          (prim:immutable-args/result? (car prim-call)))
        prim-calls))

    ;; Check each pair of primitive call / corresponding lambda arg,
    ;; and verify that if the primitive call creates a new mutable
    ;; object, that only one instance of the object will be created.
    (define (one-instance-of-new-mutable-obj? prim-calls lam-formals)
      (let ((calls/args (map list prim-calls lam-formals)))
        (call/cc 
          (lambda (return)
            (for-each
              (lambda (call/arg)
                (let ((call (car call/arg))
                      (arg (cadr call/arg)))
                  ;; Cannot inline prim call if the arg is used
                  ;; more than once and it creates a new mutable object,
                  ;; because otherwise if the object is mutated then
                  ;; only one of the instances will be affected.
                  (if (and (prim-call? call)
                           (prim-creates-mutable-obj? (car call))
                           ;; Make sure arg is not used more than once
                           (with-var arg (lambda (var)
                             (> (adbv:app-arg-count var) 1)))
                      )
                      (return #f))))
              calls/args)
            #t))))

    ;; Find variables passed to a primitive
    (define (prim-call->arg-variables exp)
      (filter symbol? (cdr exp)))

    ;; Helper for the next function
    (define (inline-prim-call? exp ivars args)
      (let ((fast-inline #t)
            (cannot-inline #f))
        ;; faster and safer but (at least for now) misses some
        ;; opportunities for optimization because it takes a global
        ;; approach rather than considering the specific variables
        ;; involved for any given optimization.
        (for-each
          (lambda (v)
            (with-var v (lambda (var)
              (if (adbv:mutated-indirectly? var)
                  (set! cannot-inline #t))
              (if (not (adbv:inlinable var))
                  (set! fast-inline #f)))))
          ivars)
;(trace:error `(DEBUG inline-prim-call ,exp ,ivars ,args ,cannot-inline ,fast-inline))
      (cond
        (cannot-inline #f)
        (else
          (if fast-inline
              ;; Fast path, no need for more analysis
              fast-inline
              ;; Fast path failed, see if we can inline anyway
              (call/cc
                (lambda (return)
                  ;(trace:error `(inline-ok? ,exp ,ivars ,args))
                  (inline-ok? exp ivars args (list #f) return)
                  (return #t))))))))

    ;; Make sure inlining a primitive call will not cause out-of-order execution
    ;; exp - expression to search
    ;; ivars - vars to be inlined
    ;; args - list of variable args (should be small)
    ;; arg-used - has a variable been used? if this is true and we find an ivar,
    ;;            it cannot be optimized-out and we have to bail.
    ;;            This is a cons "box" so it can be mutated.
    ;; return - call into this continuation to return early
    (define (inline-ok? exp ivars args arg-used return)
      ;(trace:error `(inline-ok? ,exp ,ivars ,args ,arg-used))
      (cond
        ((ref? exp)
         (cond
          ((member exp args)
           (set-car! arg-used #t))
          ((member exp ivars)
           ;;(trace:error `(inline-ok? return #f ,exp ,ivars ,args))
           (return #f))
          (else 
           #t)))
        ((ast:lambda? exp)
         (for-each
          (lambda (e)
            (inline-ok? e ivars args arg-used return))
          (ast:lambda-formals->list exp))
         (for-each
          (lambda (e)
            (inline-ok? e ivars args arg-used return))
          (ast:lambda-body exp)))
        ((const? exp) #t)
        ((quote? exp) #t)
        ((define? exp)
         (inline-ok? (define->var exp) ivars args arg-used return)
         (inline-ok? (define->exp exp) ivars args arg-used return))
        ((set!? exp)
         (inline-ok? (set!->var exp) ivars args arg-used return)
         (inline-ok? (set!->exp exp) ivars args arg-used return))
        ((if? exp)
          (if (not (ref? (if->condition exp)))
              (inline-ok? (if->condition exp) ivars args arg-used return))
          (inline-ok? (if->then exp) ivars args arg-used return)
          (inline-ok? (if->else exp) ivars args arg-used return))
        ((app? exp)
         (cond
          ((and (prim? (car exp))
                (not (prim:mutates? (car exp))))
           ;; If primitive does not mutate its args, ignore if ivar is used
           (for-each
            (lambda (e)
              (if (not (ref? e))
                  (inline-ok? e ivars args arg-used return)))
            (reverse (cdr exp))))
          (else
           (for-each
            (lambda (e)
              (inline-ok? e ivars args arg-used return))
            (reverse exp))))) ;; Ensure args are examined before function
        (else
          (error `(Unexpected expression passed to inline prim check ,exp)))))

    ;; Similar to (inline-ok?) above, but this makes a single pass to
    ;; figure out which refs can be inlined and which ones are mutated or
    ;; otherwise used in such a way that they cannot be safely inlined.
    ;;
    ;; exp - expression to analyze
    ;; args - list of formals for the current lambda
    (define (analyze:find-inlinable-vars exp args)
      ;(trace:error `(inline-ok? ,exp ,ivars ,args ,arg-used))
      (cond
        ((ref? exp)
         ;; Ignore the current lambda's formals
         (when (not (member exp args))
           ;; If the code gets this far, assume we came from a place
           ;; that does not allow the var to be inlined. We need to
           ;; explicitly white-list variables that can be inlined.
; (trace:error `(DEBUG not inlinable ,exp ,args))
           (with-var exp (lambda (var)
             (adbv:set-inlinable! var #f)))))
        ((ast:lambda? exp)
         ;; Pass along immediate lambda args, and ignore if they
         ;; are used??? sort of makes sense because we want to inline
         ;; function arguments by replacing lambda args with the actual
         ;; arguments.
         ;;
         ;; This may still need some work to match what is going on 
         ;; in inline-ok.
         (let ((formals (ast:lambda-formals->list exp)))
           (for-each
            (lambda (e)
              ;; TODO: experimental change, append args to formals instead
              ;; of just passing formals along
              ;(analyze:find-inlinable-vars e (append formals args)))
;; try this for now, do a full make then re-make and verify everything works
              (analyze:find-inlinable-vars e formals))
            (ast:lambda-body exp))))
        ((const? exp) #t)
        ((quote? exp) #t)
        ((define? exp)
         (analyze:find-inlinable-vars (define->var exp) args)
         (analyze:find-inlinable-vars (define->exp exp) args))
        ((set!? exp)
         (analyze:find-inlinable-vars (set!->var exp) args)
         (analyze:find-inlinable-vars (set!->exp exp) args))
        ((if? exp)
          (if (not (ref? (if->condition exp)))
              (analyze:find-inlinable-vars (if->condition exp) args))
          (analyze:find-inlinable-vars (if->then exp) args)
          (analyze:find-inlinable-vars (if->else exp) args))
        ((app? exp)
         (cond
          ((and (prim? (car exp))
                (not (prim:mutates? (car exp))))
           ;; If primitive does not mutate its args, ignore if ivar is used
           (for-each
            (lambda (e)
              (if (not (ref? e))
                  (analyze:find-inlinable-vars e args)))
            (cdr exp)))
            ;(reverse (cdr exp))))
          ;; If primitive mutates its args, ignore ivar if it is not mutated
          ((and (prim? (car exp))
                (prim:mutates? (car exp))
                (> (length exp) 1))
           (analyze:find-inlinable-vars (cadr exp) args)
           ;; First param is always mutated
           (for-each
            (lambda (e)
              (if (not (ref? e))
                  (analyze:find-inlinable-vars e args)))
            (cddr exp)))
          ((and (not (prim? (car exp)))
                (ref? (car exp)))
           (define pure-fnc #f)
           (define calling-cont #f)
           (define ref-formals '())
           ;; Does ref refer to a pure function (no side effects)?
           (let ((var (adb:get/default (car exp) #f)))
            (if var
                (let ((lid (adbv:defines-lambda-id var))
                      (assigned-val (adbv:assigned-value var)))
                  (cond
                   (lid
                    (with-fnc! lid (lambda (fnc)
                      (if (not (adbf:side-effects fnc))
                          (set! pure-fnc #t)))))
                   ((ast:lambda? assigned-val)
                    (with-fnc! (ast:lambda-id assigned-val) (lambda (fnc)
                      (if (not (adbf:side-effects fnc))
                          (set! pure-fnc #t)))))
                   ;; Experimental - if a cont, execution will leave fnc anyway,
                   ;; so inlines there should be safe
                   ((adbv:cont? var)
                    (set! calling-cont #t))
                   ))))
           ;;
           (with-var (car exp) (lambda (var)
             (let ((val (adbv:assigned-value var)))
              (cond
               ((ast:lambda? val)
                (set! ref-formals (ast:lambda-formals->list val)))
               ((and (pair? val) 
                     (ast:lambda? (car val)) 
                     (null? (cdr val)))
                (set! ref-formals (ast:lambda-formals->list (car val))))
           ))))
;(trace:error `(DEBUG ref app ,(car exp) ,(cdr exp) ,ref-formals))
           (cond
            ((or pure-fnc calling-cont)
              (for-each
                (lambda (e)
                  ;; Skip refs since fnc is pure and cannot change them
                  (if (not (ref? e))
                      (analyze:find-inlinable-vars e args)))
                exp))
            ;; TODO: how do you know if it is the same function, or just
            ;; another function with the same formals?
            ((= (length ref-formals) (length (cdr exp)))
             (analyze:find-inlinable-vars (car exp) args)
             (for-each
              (lambda (e a)
                ;; Optimization:
                ;; Ignore if an argument to a function is passed back 
                ;; to another call to the function as the same parameter.
                (if (not (equal? e a))
                    (analyze:find-inlinable-vars e args)))
              (cdr exp)
              ref-formals))
            (else
             (for-each
              (lambda (e)
                (analyze:find-inlinable-vars e args))
              exp))))
          (else
           (for-each
            (lambda (e)
              (analyze:find-inlinable-vars e args))
            exp))))
            ;(reverse exp))))) ;; Ensure args are examined before function
        (else
          (error `(Unexpected expression passed to find inlinable vars ,exp)))))

    (define (beta-expand/called-once? exp)
      (beta-expand/opts? exp #t))

    (define (beta-expand? exp)
      (beta-expand/opts? exp #f))

    (define (beta-expand/opts? exp called-once?)
      (cond
        ((and (app? exp)
              (ref? (car exp)))
         (with-var (car exp) (lambda (var)
           (let* ((fnc* (adbv:assigned-value var))
                  (fnc (if (and (pair? fnc*)
                                (ast:lambda? (car fnc*)))
                           (car fnc*)
                           fnc*)))
             (and (ast:lambda? fnc)
                  (or (not called-once?)
                      (= 1 (adbv:app-fnc-count var)))
                  (not (adbv:reassigned? var))
                  (not (fnc-depth>? (ast:lambda-body fnc) 4))))
           )))
        (else #f)))

    (define (fnc-depth>? exp depth)
      (call/cc
        (lambda (return)
          (define (scan exp depth)
            (if (zero? depth) (return #t))
            (cond
              ((ast:lambda? exp)
               (scan (ast:lambda-body exp) (- depth 1)))
              ((quote? exp) #f)
              ((app? exp)
               (for-each 
                (lambda (e)
                  (scan e (- depth 1)))
                exp))
              (else #f)))
          (scan exp depth)
          (return #f))))

    ;; Check app and beta expand if possible, else just return given code
    (define (beta-expand-app exp)
      (let* ((args (cdr exp))
             (var (adb:get/default (car exp) #f))
             ;; Function definition, or #f if none
             (fnc* (if var (adbv:assigned-value var) #f))
             (fnc (if (and (pair? fnc*)
                           (ast:lambda? (car fnc*)))
                      (car fnc*)
                      fnc*))
             (formals (if (ast:lambda? fnc) (ast:lambda-args fnc) '()))
             ;;; First formal, or #f if none
             ;(maybe-cont (if (and (list? formals) (pair? formals))
             ;                (car formals)
             ;                #f))
             ;;; function's continuation symbol, or #f if none
             ;(cont (if maybe-cont
             ;          (with-var maybe-cont (lambda (var)
             ;            (if (adbv:cont? var) maybe-cont #f)))
             ;          #f))
            )
;(trace:error `(JAE beta expand ,exp ,var ,fnc ,formals ,cont))
        (cond
         ;; TODO: what if fnc has no cont? do we need to handle differently?
         ((and (ast:lambda? fnc)
               (not (adbv:reassigned? var)) ;; Failsafe
               (not (adbv:cont? var)) ;; TEST, don't delete a continuation
               (list? formals)
               (= (length args) (length formals)))
          ;(trace:error `(JAE DEBUG beta expand ,exp))
          (beta-expansion-app exp fnc) ; exp
         )
         (else exp)))) ;; beta expansion failed

    ;; Replace function call with body of fnc
    (define (beta-expansion-app exp fnc)
      ;; Mapping from a formal => actual arg
      (define formals/actuals 
        (map cons (ast:lambda-args fnc) (cdr exp)))
      (define (replace ref)
        (let ((r (assoc ref formals/actuals)))
            (if r (cdr r) ref)))
      (define (scan exp)
        (cond
         ((ast:lambda? exp)
          (ast:%make-lambda
            (ast:lambda-id exp)
            (ast:lambda-args exp)
            (scan (ast:lambda-body exp))
            (ast:lambda-has-cont exp)))
         ((ref? exp)
          (replace exp))
         ((quote? exp)
          exp)
         ((app? exp)
          (map scan exp))
         (else exp)))
      (scan (car (ast:lambda-body fnc))))

    ;; Full beta expansion phase, make a pass over all of the program's AST
    (define (opt:beta-expand exp)
;(write `(DEBUG opt:beta-expand ,exp)) (newline)
      (cond
       ((ast:lambda? exp)
        (ast:%make-lambda
          (ast:lambda-id exp)
          (ast:lambda-args exp)
          (opt:beta-expand (ast:lambda-body exp))
          (ast:lambda-has-cont exp)))
       ((quote? exp) exp)
       ((const? exp) exp)
       ((ref? exp) exp)
       ((define? exp)
        `(define ,(define->var exp)
                 ,@(opt:beta-expand (define->exp exp))))
       ((set!? exp)
        `(set! ,(set!->var exp)
               ,(opt:beta-expand (set!->exp exp))))
       ((if? exp)       
        `(if ,(opt:beta-expand (if->condition exp))
             ,(opt:beta-expand (if->then exp))
             ,(opt:beta-expand (if->else exp))))
       ((app? exp)
        (let ((code (if (beta-expand? exp)
                        (beta-expand-app exp)
                        exp)))
          (map opt:beta-expand code)))
       (else exp)))

    (define (analyze-cps exp)
      (analyze-find-lambdas exp -1)
      (analyze-lambda-side-effects exp -1)
      (analyze-lambda-side-effects exp -1) ;; 2nd pass guarantees lambda purity
      (analyze exp -1) ;; Top-level is lambda ID -1
      (analyze2 exp) ;; Second pass
      (analyze:find-inlinable-vars exp '()) ;; Identify variables safe to inline
    )

    ;; NOTES:
    ;;
    ;; TODO: run CPS optimization (not all of these phases may apply)
    ;; phase 1 - constant folding, function-argument expansion, beta-contraction of functions called once,
    ;;           and other "contractions". some of this is already done in previous phases. we will leave
    ;;           that alone for now
    ;; phase 2 - beta expansion
    ;; phase 3 - eta reduction
    ;; phase 4 - hoisting
    ;; phase 5 - common subexpression elimination
    ;; TODO: re-run phases again until program is stable (less than n opts made, more than r rounds performed, etc)
    ;; END notes

    (define (optimize-cps ast)
      (adb:clear!)
      (analyze-cps ast)
      (trace:info "---------------- cps analysis db:")
      (trace:info (adb:get-db))
      (opt:beta-expand
        (opt:inline-prims 
          (opt:contract ast)))
    )

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

(define (closure-convert exp globals . opts)
 (let ((optimization-level 2))
   (if (pair? opts)
       (set! optimization-level (car opts)))
   (_closure-convert exp globals optimization-level)))

(define (_closure-convert exp globals optimization-level)
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
       (cond
         ((lambda? fn)
          (cond
            ;; If the lambda argument is not used, flag so the C code is 
            ;; all generated within the same function
            ((and (> optimization-level 0)
                  (eq? (lambda-formals-type fn) 'args:fixed)
                  (pair? (lambda-formals->list fn))
                  (with-var 
                    (car (lambda-formals->list fn))
                    (lambda (var)
                      (zero? (adbv:ref-count var))))
                  ;; Non-CPS args
                  (every
                    (lambda (x)
                      (or (not (pair? x)) ;; Should never happen
                          (and (prim-call? x)
                               (not (prim:cont? (car x))))))
                    args))
             `(Cyc-seq
               ,@args
               ,@(map cc (lambda->exp fn))))
            (else
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
                      ,@args))))))
         (else
           (let ((f (cc fn)))
            `((%closure-ref ,f 0)
              ,f
              ,@args))))))
    (else                
      (error "unhandled exp: " exp))))
  (cc exp))

 `(lambda ()
    ,(convert exp #f '())))

))
