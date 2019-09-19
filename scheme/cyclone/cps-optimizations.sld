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
          (scheme write)
          (scheme cyclone util)
          (scheme cyclone ast)
          (scheme cyclone primitives)
          (scheme cyclone transforms)
          (srfi 2)
          (srfi 69))
  (export
      closure-convert 
      pos-in-list 
      inlinable-top-level-lambda?
      optimize-cps 
      analyze-cps
      analyze-find-lambdas
      analyze:find-named-lets
      analyze:find-direct-recursive-calls
      analyze:find-known-lambdas
      analyze:find-inlinable-vars
      ;analyze-lambda-side-effects
      opt:renumber-lambdas!
      opt:add-inlinable-functions
      opt:contract
      opt:inline-prims
      opt:beta-expand
      opt:local-var-reduction
      adb:clear!
      adb:get
      adb:get/default
      adb:set!
      adb:get-db
      adb:lambda-ids
      adb:max-lambda-id
      simple-lambda?
      one-instance-of-new-mutable-obj?
      ;; Analysis - well-known lambdas
      well-known-lambda
      analyze:find-known-lambdas
      ;; Analysis - validation
      validate:num-function-args
      ;; Analyze variables
      adb:make-var
      %adb:make-var
      adb:variable?
      adbv:global?  
      adbv:set-global!
      adbv:defined-by 
      adbv:set-defined-by!
      adbv:mutated-by-set? 
      adbv:set-mutated-by-set!
      adbv:reassigned? 
      adbv:set-reassigned!
      adbv:assigned-value
      adbv:set-assigned-value!
      adbv:const? 
      adbv:set-const!
      adbv:const-value
      adbv:set-const-value!
      adbv:ref-count
      adbv:set-ref-by-and-count!
      adbv:ref-by
      adbv:def-in-loop? 
      adbv:set-def-in-loop!
      adbv:ref-in-loop? 
      adbv:set-ref-in-loop!
      adbv:direct-rec-call? 
      adbv:set-direct-rec-call!
      adbv:self-rec-call? 
      adbv:set-self-rec-call!
      adbv:app-fnc-count
      adbv:set-app-fnc-count!
      adbv:app-arg-count
      adbv:set-app-arg-count!
      adbv:cannot-inline
      adbv:set-cannot-inline!
      adbv:inlinable
      adbv:set-inlinable!
      adbv:mutated-indirectly
      adbv:set-mutated-indirectly!
      adbv:cont? adbv:set-cont!
      with-var
      with-var!
      ;; Analyze functions
      adb:make-fnc
      %adb:make-fnc
      adb:function?
      adbf:simple adbf:set-simple!
      adbf:all-params adbf:set-all-params!
      adbf:unused-params adbf:set-unused-params!
      adbf:assigned-to-var adbf:set-assigned-to-var!
      adbf:side-effects adbf:set-side-effects!
      adbf:well-known adbf:set-well-known!
      adbf:cgen-id adbf:set-cgen-id!
      adbf:closure-size adbf:set-closure-size!
      adbf:self-closure-index adbf:set-self-closure-index!
      adbf:calls-self? adbf:set-calls-self!
      adbf:vars-mutated-by-set
      adbf:set-vars-mutated-by-set!
      with-fnc
      with-fnc!
      ;; Wrap mutables
      clear-mutables
      mark-mutable
      is-mutable? 
      analyze-mutable-variables 
      wrap-mutables 
      mark-mutated-loop-var
      mutated-loop-var?
  )
  (include "cps-opt-local-var-redux.scm")
  (include "cps-opt-analyze-call-graph.scm")
  (include "cps-opt-memoize-pure-fncs.scm")
  (begin
    ;; The following two defines allow non-CPS functions to still be considered
    ;; for certain inlining optimizations.
    (define *inlinable-functions* '())
    (define (opt:add-inlinable-functions lis)
      (set! *inlinable-functions* lis))

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
    (define *adb-call-graph* (make-hash-table))
    (define (adb:get-db) *adb*)
    (define (adb:clear!)
      (set! *adb* (make-hash-table))
      (set! *adb-call-graph* (make-hash-table))
    )
    (define (adb:get key) (hash-table-ref *adb* key))
    (define (adb:get/default key default) (hash-table-ref/default *adb* key default))
    (define (adb:lambda-ids)
      (filter number? (hash-table-keys *adb*)))
    (define (adb:max-lambda-id)
      (foldl max 0 (adb:lambda-ids)))
    (define (adb:set! key val) (hash-table-set! *adb* key val))
    (define-record-type <analysis-db-variable>
      (%adb:make-var 
        global 
        defined-by 
        defines-lambda-id
        const const-value  
        ref-count ref-by
        mutated-by-set
        reassigned assigned-value 
        app-fnc-count app-arg-count
        cannot-inline
        inlinable mutated-indirectly
        cont
        def-in-loop
        ref-in-loop
        direct-rec-call
        self-rec-call
      )
      adb:variable?
      (global adbv:global? adbv:set-global!)
      (defined-by adbv:defined-by adbv:set-defined-by!)
      (defines-lambda-id adbv:defines-lambda-id adbv:set-defines-lambda-id!)
      (const adbv:const? adbv:set-const!)
      (const-value adbv:const-value adbv:set-const-value!)
      (ref-count adbv:ref-count %adbv:set-ref-count!)
      (ref-by adbv:ref-by %adbv:set-ref-by!)
      (mutated-by-set adbv:mutated-by-set? adbv:set-mutated-by-set!)
      ;; TODO: need to set reassigned flag if variable is SET, however there is at least
      ;; one exception for local define's, which are initialized to #f and then assigned
      ;; a single time via set
      (reassigned adbv:reassigned? adbv:set-reassigned!)
      (assigned-value adbv:assigned-value adbv:set-assigned-value!)
      ;; Number of times variable appears as an app-function
      (app-fnc-count adbv:app-fnc-count adbv:set-app-fnc-count!)
      ;; Number of times variable is passed as an app-argument
      (app-arg-count adbv:app-arg-count adbv:set-app-arg-count!)
      ;; Variable cannot be inlined
      (cannot-inline adbv:cannot-inline adbv:set-cannot-inline!)
      ;; Can a ref be safely inlined?
      (inlinable adbv:inlinable adbv:set-inlinable!)
      ;; Is the variable mutated indirectly? (EG: set-car! of a cdr)
      (mutated-indirectly adbv:mutated-indirectly adbv:set-mutated-indirectly!)
      (cont adbv:cont? adbv:set-cont!)
      ;; Following two indicate if a variable is defined/referenced in a loop
      (def-in-loop adbv:def-in-loop? adbv:set-def-in-loop!)
      (ref-in-loop adbv:ref-in-loop? adbv:set-ref-in-loop!)
      ;; Does a top-level function directly call itself?
      (direct-rec-call adbv:direct-rec-call? adbv:set-direct-rec-call!)
      ;; Does a function call itself?
      (self-rec-call adbv:self-rec-call? adbv:set-self-rec-call!)
    )

    (define (adbv:set-ref-by-and-count! var lambda-id)
      (let ((ref-bys (adbv:ref-by var)))
        ;(when (not (member lambda-id ref-bys)) ;; Assume low ref-by count
          (%adbv:set-ref-count! var (+ 1 (adbv:ref-count var)))
          (%adbv:set-ref-by! var (cons lambda-id ref-bys)))) ;)

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
      (%adb:make-var 
        '?  ; global 
        '?  ; defined-by 
        #f  ; defines-lambda-id
        #f  ; const 
        #f  ; const-value  
        0   ; ref-count 
        '() ; ref-by             
        #f  ; mutated-by-set
        #f  ; reassigned 
        #f  ; assigned-value 
        0   ; app-fnc-count 
        0   ; app-arg-count
        #f  ; cannot-inline
        #t  ; inlinable 
        '() ; mutated-indirectly
        #f  ; cont
        #f  ; def-in-loop
        #f  ; ref-in-loop
        #f  ; direct-rec-call
        #f  ; self-rec-call
      ))

    (define-record-type <analysis-db-function>
      (%adb:make-fnc 
       simple 
       all-params
       unused-params 
       assigned-to-var 
       side-effects
       well-known
       cgen-id
       closure-size
       self-closure-index
       calls-self
       vars-mutated-by-set
      )
      adb:function?
      (simple adbf:simple adbf:set-simple!)
      (all-params adbf:all-params adbf:set-all-params!)
      (unused-params adbf:unused-params adbf:set-unused-params!)
      (assigned-to-var adbf:assigned-to-var adbf:set-assigned-to-var!)
      (side-effects adbf:side-effects adbf:set-side-effects!)
      ;; From Dybvig's Optimizing Closures in O(0) Time paper:
      ;; A procedure is known at a call site if the call site provably invokes
      ;; that procedure's lambda-expression and only that lambda-expression. A
      ;; well-known procedure is one whose value is never used except at call
      ;; sites where it is known.
      (well-known adbf:well-known adbf:set-well-known!)
      ;; Store internal ID generated for the lambda by the cgen module
      (cgen-id adbf:cgen-id adbf:set-cgen-id!)
      ;; Number of elements in the function's closure
      (closure-size adbf:closure-size adbf:set-closure-size!)
      ;; Index of the function in its closure, if applicable
      (self-closure-index adbf:self-closure-index adbf:set-self-closure-index!)
      ;; Does this function call itself?
      (calls-self adbf:calls-self? adbf:set-calls-self!)
      ;; Variables this function mutates via (set!)
      (vars-mutated-by-set adbf:vars-mutated-by-set adbf:set-vars-mutated-by-set!)
    )
    (define (adb:make-fnc)
      (%adb:make-fnc 
       '?   ;; simple
       #f   ;; all-params
       '?   ;; unused-params
       '()  ;; assigned-to-var
       #f   ;; side-effects
       #f   ;; well-known
       #f   ;; cgen-id
       -1   ;; closure-size
       -1   ;; self-closure-index
       #f   ;; calls-self
      '()   ;; vars-mutated-by-set
      ))

    ;; A constant value that cannot be mutated
    ;; A variable only ever assigned to one of these could have all
    ;; instances of itself replaced with the value.
    (define (const-atomic? exp)
      (or (integer? exp)
          (real? exp)
          (complex? exp)
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
                       (and (not (equal? 'Cyc-seq v))
                            (not (prim? v))))
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
    (define (analyze exp scope-sym lid)
;(trace:error `(analyze ,scope-sym ,lid ,exp ,(app? exp)))
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
               (analyze expr scope-sym id))
             (ast:lambda-body exp))
           ;; Keep track of mutations made by child lambda's
           (when (> lid 0)
             (with-fnc id (lambda (inner-fnc)
               (let ((vars-set (adbf:vars-mutated-by-set inner-fnc)))
                 (when (pair? vars-set)
                  (with-fnc! lid (lambda (outer-fnc)
                    (adbf:set-vars-mutated-by-set!
                      outer-fnc
                      (append vars-set (adbf:vars-mutated-by-set outer-fnc))))))))))
        ))
        ((const? exp) #f)
        ((quote? exp) #f)
        ((ref? exp)
         (let ((var (adb:get/default exp (adb:make-var))))
          (adbv:set-ref-by-and-count! var lid)
         ))
        ((define? exp)
         ;(let ((var (adb:get/default (define->var exp) (adb:make-var))))
         (with-var! (define->var exp) (lambda (var)
           (adbv:set-defined-by! var lid)
           (adbv:set-ref-by-and-count! var lid)
           (adbv-set-assigned-value-helper! (define->var exp) var (define->exp exp))
           (adbv:set-const! var #f)
           (adbv:set-const-value! var #f)))
         (analyze (define->exp exp) (define->var exp) lid))
        ((set!? exp)
         (when (ref? (set!->var exp))
           (with-fnc! lid (lambda (fnc)
            (adbf:set-vars-mutated-by-set!
              fnc
              (cons (set!->var exp) (adbf:vars-mutated-by-set fnc))))))
         ;(let ((var (adb:get/default (set!->var exp) (adb:make-var))))
         (with-var! (set!->var exp) (lambda (var)
           (if (adbv:assigned-value var)
               (adbv:set-reassigned! var #t))
           (adbv:set-mutated-by-set! var #t)
           (adbv-set-assigned-value-helper! (set!->var exp) var (set!->exp exp))
           (adbv:set-ref-by-and-count! var lid)
           (adbv:set-const! var #f)
           (adbv:set-const-value! var #f)))
         (analyze (set!->exp exp) scope-sym lid))
        ((if? exp)       `(if ,(analyze (if->condition exp) scope-sym lid)
                              ,(analyze (if->then exp) scope-sym lid)
                              ,(analyze (if->else exp) scope-sym lid)))
        
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
          ((and (prim:mutates? (car exp))
                ;; loop3-dev WIP step #1 - do not immediately reject these prims
                ;(not (member (car exp) '(vector-set!))) ;; TODO: experimental
           )
           (let ((e (cadr exp)))
            (when (ref? e)
              (with-var! e (lambda (var)
                (adbv:set-cannot-inline! var #t)))
              (with-var e (lambda (var)
                (if (adbv:assigned-value var)
                    (set! e (adbv:assigned-value var))))))
           ;(trace:error `(find-indirect-mutations ,e))
           (find-indirect-mutations e scope-sym))))

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
             (analyze e scope-sym lid))
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
;          (adbv:set-ref-by-and-count! var lid)
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
         (validate:num-function-args exp) ;; Extra validation
         (for-each (lambda (e) (analyze2 e)) exp))
        (else #f)))

    (define (find-indirect-mutations exp scope-sym)
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
           (adbv:set-mutated-indirectly! 
             var 
             (cons scope-sym (adbv:mutated-indirectly var))))))
        ;((define? exp)
        ; ;(let ((var (adb:get/default (define->var exp) (adb:make-var))))
        ;   (analyze2 (define->exp exp)))
        ;((set!? exp)
        ; ;(let ((var (adb:get/default (set!->var exp) (adb:make-var))))
        ;   (analyze2 (set!->exp exp)))
        ((if? exp)       `(if ,(find-indirect-mutations (if->condition exp) scope-sym)
                              ,(find-indirect-mutations (if->then exp) scope-sym)
                              ,(find-indirect-mutations (if->else exp) scope-sym)))
        ; Application:
        ((app? exp)
         (for-each
           (lambda (e)
             (find-indirect-mutations e scope-sym))
           (cdr exp)))
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
                (fnc (adb:get/default id #f)))
           (if (and fnc (adbf:simple fnc))
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
             (set! exp (beta-expand-app exp #f)))
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
               ;; Perform constant folding if possible
               (if (and (prim-call? exp)
                        (precompute-prim-app? result))
                   (with-handler ;; Safety check, keep going if eval fails
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
    (define (opt:inline-prims exp scope-sym . refs*)
      (let ((refs (if (null? refs*)
                      (make-hash-table)
                      (car refs*))))
;(trace:error `(opt:inline-prims ,exp ,scope-sym))
        (cond
          ((ref? exp) 
           ;; Replace lambda variables, if necessary
           (let ((key (hash-table-ref/default refs exp #f)))
             (if key
                 (opt:inline-prims key scope-sym refs)
                 exp)))
          ((ast:lambda? exp)
           (ast:%make-lambda
            (ast:lambda-id exp)
            (ast:lambda-args exp)
            (map (lambda (b) (opt:inline-prims b scope-sym refs)) (ast:lambda-body exp))
            (ast:lambda-has-cont exp)))
          ((const? exp) exp)
          ((quote? exp) exp)
          ((define? exp)
           `(define ,(define->var exp)
                    ,@(opt:inline-prims (define->exp exp) (define->var exp) refs))) ;; TODO: map????
          ((set!? exp)
           `(set! ,(set!->var exp)
                  ,(opt:inline-prims (set!->exp exp) scope-sym refs)))
          ((if? exp)       
           (cond
            ((not (if->condition exp))
             (opt:inline-prims (if->else exp) scope-sym refs)) ;; Always false, so replace with else
            ((const? (if->condition exp))
             (opt:inline-prims (if->then exp) scope-sym refs)) ;; Always true, replace with then
            (else
              `(if ,(opt:inline-prims (if->condition exp) scope-sym refs)
                   ,(opt:inline-prims (if->then exp) scope-sym refs)
                   ,(opt:inline-prims (if->else exp) scope-sym refs)))))
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
                            ;;(ref? arg) ;; DEBUG
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
                    scope-sym
                    (prim-calls->arg-variables (cdr exp))
                    (ast:lambda-formals->list (car exp)))))
                    #t)
                  ;; Double-check parameter can be optimized-out
                  (every
                    (lambda (param)
                      (with-var param (lambda (var)
;(trace:error `(DEBUG ,param ,(adbv:ref-by var)))
                        (and 
                          ;; If param is referenced in a loop (but defined outside)
                          ;; do not inline function into the loop
                          (or (not (adbv:ref-in-loop? var))
                              (adbv:def-in-loop? var))
                          ;; If param is never referenced, then prim is being
                          ;; called for side effects, possibly on a global
                          (not (null? (adbv:ref-by var)))
                          ;; Need to keep variable because it is mutated
                          (not (adbv:reassigned? var))

                          ;; Make sure param is not computed by vars that may be mutated
                          (inline-ok-from-call-graph? param *adb-call-graph*)
                    ))))
                    (ast:lambda-formals->list (car exp)))
                  ;; Check all args are valid primitives that can be inlined
                  (every
                    (lambda (arg)
                      (and (prim-call? arg)
                           ;; Do not inline functions that are looping over lists, seems counter-productive
                           (not (member (car arg) '( member assoc Cyc-fast-member Cyc-fast-assoc assq assv memq memv)))
                           (not (prim:cont? (car arg)))))
                    (cdr exp))
                  ;; Disallow primitives that allocate a new obj,
                  ;; because if the object is mutated all copies
                  ;; must be modified. 
                  (one-instance-of-new-mutable-obj?
                    (cdr exp)
                    (ast:lambda-formals->list (car exp)))
                  (or
                    (prim-calls-inlinable? (cdr exp))

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
                      scope-sym
                      (prim-calls->arg-variables (cdr exp))
                      (ast:lambda-formals->list (car exp))))))
             )
             (let ((args (cdr exp)))
               (for-each
                (lambda (param)
                  (hash-table-set! refs param (car args))
                  (set! args (cdr args)))
                (ast:lambda-formals->list (car exp))))
             (opt:inline-prims (car (ast:lambda-body (car exp))) scope-sym refs))
            ;; Lambda with a parameter that is never used; sequence code instead to avoid lambda
            ((and (ast:lambda? (car exp))
                  (every
                    (lambda (arg)
                      (or (not (prim-call? arg))
                          (not (prim:cont? (car arg)))))
                    (cdr exp))
                  (every
                    (lambda (param)
                      (with-var param (lambda (var)
                        (null? (adbv:ref-by var)))))
                    (ast:lambda-formals->list (car exp)))
             )
             (opt:inline-prims 
               `(Cyc-seq 
                  ,@(cdr exp)
                  ,@(ast:lambda-body (car exp))) 
               scope-sym 
               refs))
            (else
              (map (lambda (e) (opt:inline-prims e scope-sym refs)) exp))))
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
          Cyc-fast-list-1
          Cyc-fast-list-2
          Cyc-fast-list-3
          Cyc-fast-list-4
          Cyc-fast-vector-2
          Cyc-fast-vector-3
          Cyc-fast-vector-4
          Cyc-fast-vector-5
          )))

    (define (prim-calls-inlinable? prim-calls)
      (every
        (lambda (prim-call)
          (and 
            (prim:immutable-args/result? (car prim-call))
            ; Issue #172 - Cannot assume that just because a primitive 
            ; deals with immutable objects that it is safe to inline.
            ; A (set!) could still mutate variables the primitive is
            ; using, causing invalid behavior.
            ;
            ; So, make sure none of the args is mutated via (set!)
            (every
              (lambda (arg)
                (or (not (ref? arg))
                    (with-var arg (lambda (var)
                      (not (adbv:mutated-by-set? var))))))
              (cdr prim-call)))
          )
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
    (define (inline-prim-call? exp scope-sym ivars args)
      (let ((fast-inline #t)
            (cannot-inline #f))
        ;; TODO: causes problems doing this here???
        ;(for-each
        ;  (lambda (v)
        ;    (with-var v (lambda (var)
        ;      (if (adbv:mutated-by-set? var)
        ;          (set! cannot-inline #t)))))
        ;  args)
        (for-each
          (lambda (v)
            (with-var v (lambda (var)
              (if (or (member scope-sym (adbv:mutated-indirectly var))
                     ;(adbv:mutated-by-set? var)  ;; TOO restrictive, only matters if set! occurs in body we
                  )                               ;; are inlining to. Also, does not catch cases where the
                                                  ;; var might be mutated by a function call outside this
                                                  ;; module (but hopefully we already catch that elsewhere).
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
              ;; faster and safer but (at least for now) misses some
              ;; opportunities for optimization because it takes a global
              ;; approach rather than considering the specific variables
              ;; involved for any given optimization. That's why we call
              ;; inline-ok? below if this is false.
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
           ;(trace:error `(inline-ok? return #f ,exp ,ivars ,args))
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
          ;; loop3-dev WIP step #2 - some args can be safely ignored
          ;((and (prim? (car exp))
          ;      (prim:mutates? (car exp))
          ;      (member (car exp) '(vector-set!))
          ; )
          ; ;; with vector-set, only arg 1 (the vector) is actually mutated
          ; ;; TODO: is this always true? do we have problems with self-recursive vecs??
          ; (inline-ok? (cadr exp) ivars args arg-used return) 
          ;)
         (else
           (when (ref? (car exp))
            (with-var (car exp) (lambda (var)
              (when (adbv:defines-lambda-id var)
                ;TODO: return #f if any ivars are members of vars-mutated-by-set from the adbf
                (with-fnc (adbv:defines-lambda-id var) (lambda (fnc)
                  (for-each
                    (lambda (ivar)
                      (if (member ivar (adbf:vars-mutated-by-set fnc))
                        (return #f))
                    )
                    ivars))))
            )))
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
          ((or
            (member (car exp) *inlinable-functions*)
            (and (prim? (car exp))
                 (not (prim:mutates? (car exp)))))
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
                  (not (adbv:self-rec-call? var))
                  (not (fnc-depth>? (ast:lambda-body fnc) 4))
                  ;(not (fnc-depth>? (ast:lambda-body fnc) 5))
                  ;; Issue here is we can run into code that calls the 
                  ;; same continuation from both if branches. In this
                  ;; case we do not want to beta-expand as a contraction
                  ;; because duplicate instances of the same code may be
                  ;; introduced, causing problems downstream.
                  (or (not called-once?)
                      (and called-once?
                           (not (contains-if? (ast:lambda-body fnc)))))
             ))
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

    (define (contains-if? exp)
      (call/cc
        (lambda (return)
          (define (scan exp)
            (cond
              ((ast:lambda? exp)  (scan (ast:lambda-body exp)))
              ((quote? exp)       #f)
              ((if? exp)          (return #t))
              ((app? exp)         (for-each scan exp))
              (else #f)))
          (scan exp)
          (return #f))))

    ;; Check app and beta expand if possible, else just return given code
    (define (beta-expand-app exp rename-lambdas)
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
;(trace:error `(JAE beta expand ,exp ,var ,fnc ,formals ))
        (cond
         ;; TODO: what if fnc has no cont? do we need to handle differently?
         ((and (ast:lambda? fnc)
               (not (adbv:reassigned? var)) ;; Failsafe
               (not (equal? fnc (adbv:assigned-value var))) ;; Do not expand recursive func
               (not (adbv:cont? var)) ;; TEST, don't delete a continuation
               (list? formals)
               (= (length args) (length formals)))
          ;(trace:error `(JAE DEBUG beta expand ,exp))
          (beta-expansion-app exp fnc rename-lambdas) ; exp
         )
         (else exp)))) ;; beta expansion failed

    ;; Replace function call with body of fnc
    (define (beta-expansion-app exp fnc rename-lambdas)
;(write `(beta-expansion-app ,exp))
;(newline)
      ;; Mapping from a formal => actual arg
      (define formals/actuals 
        (map cons (ast:lambda-args fnc) (cdr exp)))
      ;; Replace ref with corresponding argument from function call being replaced
      (define (replace ref renamed)
        (let ((r (assoc ref formals/actuals)))
;(write `(DEBUG2 replace ,ref ,renamed ,r))
;(newline)
            (if (and r 
                     (not (eq? (car r) (cdr r)))) ;; Prevent an inf loop
                (scan (cdr r) renamed) 
                ;ref
                (let ((rn (assoc ref renamed)))
                  (if rn
                      (cdr rn)
                      ref)))))
      (define (scan exp renamed)
        (cond
         ((ast:lambda? exp)
          (if rename-lambdas
            (let* ((args (ast:lambda-formals->list exp))
                   (ltype (ast:lambda-formals-type exp))
                   (a-lookup (map (lambda (a) (cons a (gensym a))) args)))
              (ast:%make-lambda
                ;; if rename-lambdas also need to rename lambda formals here and
                ;; setup and a-lookup to replace any refs with the renamed formal. the
                ;; problem is, if we don't do this, there will be multiple lambda's with
                ;; the same arg, which causes problems when the optimizer tries to replace
                ;; one with its value, since different instances will have different values
                (ast:get-next-lambda-id!)
                (list->lambda-formals
                   (map (lambda (p) (cdr p)) a-lookup)  
                   ltype)
                (scan (ast:lambda-body exp) (append a-lookup renamed))
                (ast:lambda-has-cont exp)))
            (ast:%make-lambda
              (ast:lambda-id exp)
              (ast:lambda-args exp)
              (scan (ast:lambda-body exp) renamed)
              (ast:lambda-has-cont exp))))
         ((ref? exp)
          (replace exp renamed))
         ((quote? exp)
          exp)
         ((app? exp)
          (map (lambda (e) (scan e renamed)) exp))
         (else exp)))
      (scan (car (ast:lambda-body fnc)) '()))

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
                        (beta-expand-app exp #t)
                        exp)))
          (map opt:beta-expand code)))
       (else exp)))

    (define (analyze-cps exp)
      (analyze:find-named-lets exp)
      (analyze:find-direct-recursive-calls exp)
      (analyze:find-recursive-calls exp)
      (analyze-find-lambdas exp -1)
      (analyze-lambda-side-effects exp -1)
      (analyze-lambda-side-effects exp -1) ;; 2nd pass guarantees lambda purity
      (analyze exp -1 -1) ;; Top-level is lambda ID -1
      (analyze2 exp) ;; Second pass
      (analyze:find-inlinable-vars exp '()) ;; Identify variables safe to inline
      (set! *adb-call-graph* (analyze:build-call-graph exp))
      (analyze:find-recursive-calls2 exp)
      ;(analyze:set-calls-self)
    )

    (define (analyze:set-calls-self)
      (let ((idents (filter symbol? (hash-table-keys *adb*))))
        (for-each
          (lambda (id)
            (let ((var (adb:get/default id #f)))
              (when (and var (adbv:self-rec-call? var))
                (and-let*
                  ((a-value (adbv:assigned-value var))
                   ((pair? a-value))
                   ((ast:lambda? (car a-value)))
                   (lid (ast:lambda-id (car a-value))))
(trace:info `(TODO ,id ,lid ,a-value))
                  (with-fnc! lid (lambda (fnc)
                    (adbf:set-calls-self! fnc #t))))
              ))
          )
          idents)))

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

    (define (optimize-cps ast add-globals! flag-set?)
      (adb:clear!)
      (analyze-cps ast)
      (trace:info "---------------- cps analysis db:")
      (trace:info (adb:get-db))
      (let ((new-ast (opt:inline-prims 
                       (opt:contract ast) -1)))
        ;; Just a hack for now, need to fix beta expand in compiler benchmark
        (when (< (length (filter define? new-ast)) 1000)
          (set! new-ast 
                (opt:beta-expand new-ast)) ;; TODO: temporarily disabled, causes problems with massive expansions 
                                           ;; in compiler benchmark, need to revist how to throttle/limit this 
                                           ;; (program size? heuristics? what else??)
        )

        ;; Memoize pure functions, if instructed
        (when (and (procedure? flag-set?) (flag-set? 'memoize-pure-functions))
          (set! new-ast (opt:memoize-pure-fncs new-ast add-globals!))
        )
        new-ast
      )
    )

;; Renumber lambdas and re-run analysis
(define (opt:renumber-lambdas! exp)
  (define (scan exp)
    (cond
     ((ast:lambda? exp)
      (ast:%make-lambda
        (ast:get-next-lambda-id!)
        (ast:lambda-args exp)
        (scan (ast:lambda-body exp))
        (ast:lambda-has-cont exp)))
     ((quote? exp)
      exp)
     ((app? exp)
      (map (lambda (e) (scan e)) exp))
     (else exp)))

  (ast:reset-lambda-ids!) ;; Convenient to start back from 1
  (let ((result (scan exp)))
    (adb:clear!)
    (analyze-cps result)
    result))

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

(define (let->vars exp)
  (map car (cadr exp)))

(define (closure-convert exp globals . opts)
 (let ((optimization-level 2))
   (if (pair? opts)
       (set! optimization-level (car opts)))
   (_closure-convert exp globals optimization-level)))

(define (_closure-convert exp globals optimization-level)
 (define (set-adb-info! id formals size)
  (with-fnc! id (lambda (fnc)
    (adbf:set-all-params! fnc formals)
    (adbf:set-closure-size! fnc size))))
 (define (convert exp self-var free-var-lst)
  (define (cc exp)
;(trace:error `(cc ,exp))
   (cond
    ((ast:lambda? exp)
     (let* ((new-self-var (gensym 'self))
            (body  (ast:lambda-body exp))
            (new-free-vars 
              (difference 
                (difference (free-vars body) (cons 'Cyc-seq (cons 'Cyc-local-set! (ast:lambda-formals->list exp))))
                globals))
            (formals (list->lambda-formals
                       (cons new-self-var (ast:lambda-formals->list exp))
                       (ast:lambda-formals-type exp)))
           )
       (set-adb-info!
         (ast:lambda-id exp)
         formals
         (length new-free-vars))
       `(%closure
          ,(ast:%make-lambda
             (ast:lambda-id exp)
             formals
             (list (convert (car body) new-self-var new-free-vars))
             (ast:lambda-has-cont exp))
          ,@(map (lambda (v) ;; TODO: splice here?
                    (cc v))
            new-free-vars))))
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
    ;; Special case now with local var redux
    ((tagged-list? 'let exp) 
     `(let 
        ,(map 
          (lambda (var/val)
            (let ((var (car var/val))
                  (val (cadr var/val)))
            `(,var ,(cc val))))
          (cadr exp))
        ,(convert
            (caddr exp) 
            self-var 
            ;; Do not closure convert the let's variables because
            ;; the previous code guarantees they are locals
            (filter (lambda (v) (not (member v (let->vars exp)))) free-var-lst)))
    )
    ((lambda? exp)   (error `(Unexpected lambda in closure-convert ,exp)))
    ((if? exp)  `(if ,@(map cc (cdr exp))))
    ((cell? exp)       `(cell ,(cc (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(cc (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(cc (set-cell!->cell exp))
                                   ,(cc (set-cell!->value exp))))
    ((app? exp)
     (let ((fn (car exp))
           (args (map cc (cdr exp))))
       (cond
         ;TODO: what about application of cyc-seq? does this only occur as a nested form? can we combine here or earlier??
         ;      I think that is what is causing cc printing to explode exponentially!
         ;((tagged-list? 'Cyc-seq fnc)
        ; (foldl (lambda (sexp acc) (cons sexp acc)) '() (reverse '(a b c (cyc-seq 1) (cyc-seq 2 ((cyc-seq 3))))))
        ; TODO: maybe just call a function to 'flatten' seq's
         ((equal? 'Cyc-seq fn)
          `(Cyc-seq ,@args))
         ((equal? 'Cyc-local-set! fn)
          `(Cyc-local-set! ,@args))
         ((ast:lambda? fn)

(let* ((replace
          (cond 
           ((and (= 2 (length exp))
                 (ast:lambda? (car exp))
                 ;#f ;; TEmporarily disabled
                 (not (cadr exp)))
            ;; Candidate, see if the var is set to a lambda
            (with-var (car (ast:lambda-formals->list (car exp))) (lambda (var)
              (cond
               ((and (adbv:mutated-by-set? var)
                     (ast:lambda? (adbv:assigned-value var)))
                (mark-mutated-loop-var (car (ast:lambda-formals->list (car exp))))
                (trace:error `(found loop var ,(car (ast:lambda-formals->list (car exp)))))
                #t)
               (else #f)))))
           (else #f)))
         (result
          (cond
            ;; If the lambda argument is not used, flag so the C code is 
            ;; all generated within the same function
            ((and #f
                  (> optimization-level 0)
                  (eq? (ast:lambda-formals-type fn) 'args:fixed)
                  (pair? (ast:lambda-formals->list fn))
                  (with-var 
                    (car (ast:lambda-formals->list fn))
                    (lambda (var)
                      (zero? (adbv:ref-count var))))
                  ;; Non-CPS args
                  (every
                    (lambda (x)
                      (or (not (pair? x)) ;; Should never happen
                          (and (prim-call? x)
                               ;; TODO: necessary for gcbench stability??? (not (prim:mutates? (car x)))
                               (not (prim:cont? (car x))))))
                    args))
             `(Cyc-seq
               ,@args
               ,@(map cc (ast:lambda-body fn))))
            (else
              (let* ((body  (ast:lambda-body fn))
                     (new-free-vars 
                       (difference
                         (difference (free-vars body) (cons 'Cyc-seq (cons 'Cyc-local-set! (ast:lambda-formals->list fn))))
                         globals))
                     (new-free-vars? (> (length new-free-vars) 0)))
                  (if new-free-vars?
                    ; Free vars, create a closure for them
                    (let* ((new-self-var (gensym 'self))
                           (formals
                             (list->lambda-formals
                                (cons new-self-var (ast:lambda-formals->list fn))
                                (ast:lambda-formals-type fn)))
                          )
                      (set-adb-info!
                        (ast:lambda-id fn)
                        formals
                        (length new-free-vars))
                      `((%closure 
                          ,(ast:%make-lambda
                             (ast:lambda-id fn)
                             formals
                             (list (convert (car body) new-self-var new-free-vars))
                             (ast:lambda-has-cont fn)
                           )
                           ,@(map (lambda (v) (cc v))
                                  new-free-vars))
                        ,@args))
                    ; No free vars, just create simple lambda
                    `(,(ast:%make-lambda 
                         (ast:lambda-id fn)
                         (ast:lambda-args fn)
                         (map cc body)
                         (ast:lambda-has-cont fn)
                       )
                      ,@args)
                    
                    ))))))


         (with-handler
          (lambda (err)
            (trace:error `(error transforming CC loop ,err))
            result)
          (cond
            ((and replace
                  (tagged-list? '%closure (car result)) ;; TODO: see above, not always a closure, what to do then?
             )
              (define (clo->lambda-body sexp)
                (car (ast:lambda-body (cadr sexp))))
              
              (let* ((outer-body (clo->lambda-body (car result))) ; (clo-call cell)
                     (inner-body (clo->lambda-body (car outer-body)))
                     (set-cell-exp (cadr inner-body))
                     (set-clo (caddr set-cell-exp))
                    )
                (set-car! (cdr inner-body) #f) ; '%Cyc-noop ;; Don't do the set
                (set-cdr! outer-body `((cell ,set-clo))) ;; Relocate the closure
                result
              ))
            (else result)))
))
         ((lambda? fn)   (error `(Unexpected lambda in closure-convert ,exp)))
         (else
           (let ((f (cc fn)))
            `((%closure-ref ,f 0)
              ,f
              ,@args))))))
    (else                
      (error "unhandled exp: " exp))))
  (cc exp))

 (ast:make-lambda
   (list)
   (list (convert exp #f '()))
   #f))

(define (analyze:find-named-lets exp)
  (define (scan exp lp)
    (cond
     ((ast:lambda? exp)
      (let* ((id (ast:lambda-id exp))
             (has-cont (ast:lambda-has-cont exp))
             (sym (string->symbol 
                    (string-append
                       "lambda-"
                       (number->string id)
                       (if has-cont "-cont" ""))))
             (args* (ast:lambda-args exp))
             (args (if (null? args*) 
                       '() 
                       (formals->list args*)))
            )
        (when lp
          (for-each
            (lambda (a)
              (with-var! a (lambda (var)
                (adbv:set-def-in-loop! var #t))))
            args))
        `(,sym ,(ast:lambda-args exp)
           ,@(map (lambda (e) (scan e lp)) (ast:lambda-body exp))))
     )
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      (when lp
        ;(trace:error `(found var ref ,exp in loop))
        (with-var! exp (lambda (var)
          (adbv:set-ref-in-loop! var #t))))
      exp)
     ((define? exp)
      `(define ,(define->var exp)
               ,@(scan (define->exp exp) lp)))
     ((set!? exp)
      `(set! ,(set!->var exp)
             ,(scan (set!->exp exp) lp)))
     ((if? exp)       
      `(if ,(scan (if->condition exp) lp)
           ,(scan (if->then exp) lp)
           ,(scan (if->else exp) lp)))
     ((app? exp)
      (cond
        ;; Detect CPS pattern without any optimizations
        ((and-let* (
           ;; Find lambda with initial #f assignment
           ((ast:lambda? (car exp)))
           ((pair? (cdr exp)))
           ((not (cadr exp)))
           ((list? (ast:lambda-args (car exp))))
           (= 1 (length (ast:lambda-args (car exp))))
           ;; Get information for continuation
           (loop-sym (car (ast:lambda-args (car exp))))
           (inner-exp (car (ast:lambda-body (car exp))))
           ((app? inner-exp))
           ((ast:lambda? (car inner-exp)))
           ;; Find the set (assumes CPS conversion)
           ((pair? (cdr inner-exp)))
           ((ast:lambda? (car inner-exp)))
           (lambda/set (car (ast:lambda-body (car inner-exp))))
           ((app? lambda/set))
           ((ast:lambda? (car lambda/set)))
           ((pair? (cdr lambda/set)))
           ((set!? (cadr lambda/set)))
           ((equal? (set!->var (cadr lambda/set)) loop-sym))
;           (unused (begin (newline) (newline) (write loop-sym) (write (ast:ast->pp-sexp (cadr lambda/set))) (newline)(newline)))
           ;; Check the set's continuation
           ((app? (car (ast:lambda-body (car lambda/set)))))
;           (unused2 (begin (newline) (newline) (write (ast:ast->pp-sexp (car lambda/set))) (newline)(newline)))
           ((equal? (caar (ast:lambda-body (car lambda/set))) loop-sym))
         )
         ;(trace:error `(found loop in ,exp))
         ;; TODO: do we want to record the lambda that is a loop?
         ;; Continue scanning, indicating we are in a loop
         (map (lambda (e) (scan e #t)) exp)
        ))
        ;; Detect optimized CPS pattern
        ((and-let* ( 
           ;; Find lambda with initial #f assignment
           ((ast:lambda? (car exp)))
           ((pair? (cdr exp)))
           ((not (cadr exp)))
           (= 1 (length (ast:lambda-args (car exp))))
           ;; Get information for continuation
           (loop-sym (car (ast:lambda-args (car exp))))
           (inner-exp (car (ast:lambda-body (car exp))))
           ((app? inner-exp))
           ((ast:lambda? (car inner-exp)))
           ;; Find the set (assumes CPS conversion)
           ((pair? (cdr inner-exp)))
           ((set!? (cadr inner-exp)))
           ((equal? (set!->var (cadr inner-exp)) loop-sym))
           ;; Check the set's continuation
           ((app? (car (ast:lambda-body (car inner-exp)))))
           ((equal? (caar (ast:lambda-body (car inner-exp))) loop-sym))
          )
         ;;(trace:error `(found loop in ,exp))
         ;; TODO: do we want to record the lambda that is a loop?
         ;; Continue scanning, indicating we are in a loop
         (map (lambda (e) (scan e #t)) exp)
        ))
        (else
          (map (lambda (e) (scan e lp)) exp))))
     (else exp)))
  (scan exp #f))

;; Find any top-level functions that call themselves directly
(define (analyze:find-direct-recursive-calls exp)
  ;; Verify the continuation is simple and there is no closure allocation
  (define (check-cont k)
    (cond
      ((ref? k) #t)
      (else #f)))

  ;; Check arguments to the top level function to make sure 
  ;; they are "safe" for further optimizations.
  ;; Right now this is very conservative.
  (define (check-args args)
    (define (check exp)
      (cond
        ((quote? exp) #t)
        ((const? exp) #t)
        ((ref? exp) #t)
        ((app? exp)
         (and 
           ;; TODO: Very conservative right now, could include more
           (member (car exp) '(car cdr))
           (check-args (cdr exp))))
        (else #f)))
    (every check args))

  (define (scan exp def-sym)
    ;(trace:info `(analyze:find-direct-recursive-calls scan ,def-sym ,exp))
    (cond
     ((ast:lambda? exp)
      ;; Reject if there are nested functions
      #f)
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      exp)
     ((define? exp) #f)
     ((set!? exp) #f)
     ((if? exp)       
      (scan (if->condition exp) def-sym) ;; OK to check??
      (scan (if->then exp) def-sym)
      (scan (if->else exp) def-sym))
     ((app? exp)
      (when (equal? (car exp) def-sym)
        (cond
         ((and
            (check-cont (cadr exp))
            (check-args (cddr exp)))
          (trace:info `("direct recursive call" ,exp ,(cadr exp) ,(check-cont (cadr exp))))
          (with-var! def-sym (lambda (var)
            (adbv:set-direct-rec-call! var #t))))
         (else
          (trace:info `("not a direct recursive call" ,exp))))))
     (else #f)))

  (if (pair? exp)
      (for-each
        (lambda (exp)
          ;;(write exp) (newline)
          (and-let* (((define? exp))
                      (def-exps (define->exp exp))
                     ((vector? (car def-exps)))
                     ((ast:lambda? (car def-exps)))
                     )
           (scan (car (ast:lambda-body (car def-exps))) (define->var exp))))
          exp))
)

;; Find functions that call themselves. This is not as restrictive 
;; as finding "direct" calls.
(define (analyze:find-recursive-calls exp)

  (define (scan exp def-sym)
    ;(trace:info `(analyze:find-recursive-calls scan ,def-sym ,exp))
    (cond
     ((ast:lambda? exp)
      (for-each
        (lambda (e)
          (scan e def-sym))
        (ast:lambda-body exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      exp)
     ((define? exp) #f) ;; TODO ??
     ((set!? exp) #f) ;; TODO ??
     ((if? exp)       
      (scan (if->condition exp) def-sym)
      (scan (if->then exp) def-sym)
      (scan (if->else exp) def-sym))
     ((app? exp)
      (when (equal? (car exp) def-sym)
        (trace:info `("recursive call" ,exp))
        (with-var! def-sym (lambda (var)
          (adbv:set-self-rec-call! var #t)))
        ))
     (else #f)))

  ;; TODO: probably not good enough, what about recursive functions that are not top-level??
  (if (pair? exp)
      (for-each
        (lambda (exp)
          ;;(write exp) (newline)
          (and-let* (((define? exp))
                      (def-exps (define->exp exp))
                     ((vector? (car def-exps)))
                     ((ast:lambda? (car def-exps)))
                     )
           (scan (car (ast:lambda-body (car def-exps))) (define->var exp))))
          exp))
)

;; Does given symbol refer to a recursive call to given lambda ID?
(define (rec-call? sym lid)
  (cond
   ((ref? sym)
    (let ((var (adb:get/default sym #f)))
      ;(trace:info 
      ;  `(rec-call? ,sym ,lid
      ;       ;; TODO: crap, these are not set yet!!!
      ;       ;; may need to consider keeping out original version of find-recursive-calls and
      ;       ;; adding a new version that does a deeper analysis
      ;       ,(if var (not (adbv:reassigned? var)) #f)
      ;       ,(if var (adbv:assigned-value var) #f)
      ;       ;,((ast:lambda? var-lam))
      ;       ,(adb:get/default lid #f)
      ;       )
      ;     )
      (and-let* (
                 ((not (equal? var #f)))
                 ((not (adbv:reassigned? var)))
                 (var-lam (adbv:assigned-value var))
                 ((ast:lambda? var-lam))
                 (fnc (adb:get/default lid #f))
                )
        ;(trace:info `(equal? ,lid ,(ast:lambda-id var-lam)))
        (equal? lid (ast:lambda-id var-lam)))))
   (else
    #f)))

;; Same as the original function, but this one is called at the end of analysis and 
;; uses data that was previously not available.
;;
;; The reason for having two versions of this is that the original is necessary for
;; beta expansion (and must remain, at least for now) and this one will provide useful
;; data for code generation.
;;
;; TODO: is the above true? not so sure anymore, need to verify that, look at optimize-cps
(define (analyze:find-recursive-calls2 exp)

  (define (scan exp def-sym lid)
    ;(trace:info `(analyze:find-recursive-calls2 scan ,def-sym ,exp ,lid))
    (cond
     ((ast:lambda? exp)
      (for-each
        (lambda (e)
          (scan e def-sym (ast:lambda-id exp)))
        (ast:lambda-body exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      exp)
     ((define? exp) #f) ;; TODO ??
     ((set!? exp)
      (for-each
        (lambda (e)
          (scan e def-sym lid))
        (cdr exp))
     )
     ((if? exp)       
      (scan (if->condition exp) def-sym lid)
      (scan (if->then exp) def-sym lid)
      (scan (if->else exp) def-sym lid))
     ((app? exp)
      (when (or ;(equal? (car exp) def-sym) TODO: def-sym is obsolete, remove it
                (rec-call? (car exp) lid))
        ;(trace:info `("recursive call" ,exp))
        (with-fnc! lid (lambda (fnc)
          (adbf:set-calls-self! fnc #t)))
        (with-var! (car exp) (lambda (var)
          (adbv:set-self-rec-call! var #t))))
      (for-each
        (lambda (e)
          (scan e def-sym lid))
        exp)
     )
     (else #f)))

  ;; TODO: probably not good enough, what about recursive functions that are not top-level??
;TODO: need to address those now, I think we have the support now via (rec-call?)
  (if (pair? exp)
      (for-each
        (lambda (exp)
          ;(trace:info `(analyze:find-recursive-calls ,exp))
          (and-let* (((define? exp))
                      (def-exps (define->exp exp))
                     ((vector? (car def-exps)))
                     ((ast:lambda? (car def-exps)))
                     (id (ast:lambda-id (car def-exps)))
                     )
           (scan (car (ast:lambda-body (car def-exps))) (define->var exp) id)
        ))
        exp))
)
;; well-known-lambda :: symbol -> Either (AST Lambda | Boolean)
;; Does the given symbol refer to a well-known lambda?
;; If so the corresponding lambda object is returned, else #f.
(define (well-known-lambda sym)
  (and *well-known-lambda-sym-lookup-tbl*
       (hash-table-ref/default *well-known-lambda-sym-lookup-tbl* sym #f)))

(define *well-known-lambda-sym-lookup-tbl* #f)

;; Scan for well-known lambdas:
;; - app of a lambda is well-known, that's easy
;; - lambda passed as a cont. If we can identify all the places the cont is
;;   called and it is not used for anything but calls, then I suppose that
;;   also qualifies as well-known.
;; - ?? must be other cases
(define (analyze:find-known-lambdas exp)
  ;; Lambda conts that are candidates for well-known functions,
  ;; we won't know until we check exactly how the cont is used...
  (define candidates (make-hash-table))
  (define scopes (make-hash-table))

  ;; Add given lambda to candidate table
  ;; ast:lam - AST Lambda object
  ;; scope-ast:lam - Lambda that is calling ast:lam
  ;; param-sym - Symbol of the parameter that the lambda is passed as
  (define (add-candidate! ast:lam scope-ast:lam param-sym)
    (hash-table-set! candidates param-sym ast:lam)
    (hash-table-set! scopes param-sym scope-ast:lam)
  )

  ;; Remove given lambda from candidate table
  ;; param-sym - Symbol representing the lambda to remove
  (define (remove-candidate param-sym)
    (hash-table-delete! candidates param-sym)
    (hash-table-delete! scopes param-sym)
  )

  ;; Determine if the expression is a call to a primitive that receives
  ;; a continuation. This is important since well-known functions cannot
  ;; be passed as such a cont.
  (define (prim-call/cont? exp)
    (let ((result (and (app? exp)
                       (prim:cont? (car exp)))))
      ;(trace:info `(prim-call/cont? ,exp ,result))
      result))

  (define (found exp . sym)
    (let ((lid (ast:lambda-id exp)))
      (if (null? sym)
          (trace:info `(found known lambda with id ,lid))
          (trace:info `(found known lambda with id ,lid sym ,(car sym))))
      (with-fnc! lid (lambda (fnc)
        (adbf:set-well-known! fnc #t)))))

  (define (scan exp scope)
    (cond
     ((ast:lambda? exp)
      (for-each
        (lambda (e)
          (scan e (ast:lambda-id exp)))
        (ast:lambda-body exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      (remove-candidate exp)
      exp)
     ((define? exp) 
      (for-each
        (lambda (e)
          (scan e -1))
        (define->exp exp)))
     ;((set!? exp) #f) ;; TODO ??
     ((if? exp)       
      (scan (if->condition exp) scope)
      (scan (if->then exp) scope)
      (scan (if->else exp) scope))
     ((app? exp)
      (cond
        ((ast:lambda? (car exp))
         (when (not (any prim-call/cont? (cdr exp)))
           (found (car exp))) ;; We immediately know these lambdas are well-known
         (let ((formals (ast:lambda-formals->list (car exp))))
           (when (and (pair? formals)
                      (pair? (cdr exp))
                      (ast:lambda? (cadr exp))
                      ;; Lambda is not well-known when called from a runtime prim
                      ;;(not (any prim-call/cont? (cdr exp)))
                 )
             (add-candidate! (cadr exp) (car exp) (car formals)))
         )
         ;; Scan the rest of the args
         (for-each
           (lambda (e)
             (scan e scope))
           exp))
        (else 
          (for-each
            (lambda (e)
              (scan e scope))
            (cond
             ((ref? (car exp))
              (let ((cand (hash-table-ref/default scopes (car exp) #f)))
                (cond
                  ;; Allow candidates to remain if they are just function calls
                  ;; and they are called by the same function that defines them
                  ((and cand
                        (equal? (ast:lambda-id cand) scope)
                        (not (any prim-call/cont? (cdr exp)))
                   )
                   (cdr exp))
                  (else 
                    exp))))
             (else
               exp))))))
     (else #f)))

;(trace:error `(update-lambda-atv! ,syms ,value))
  (scan exp -1)
  ;; Record all well-known lambdas that were found indirectly
  (for-each
    (lambda (sym/lamb)
      (found (cdr sym/lamb) (car sym/lamb)))
    (hash-table->alist candidates))
  ;; Save the candidate list so we can use it to lookup
  ;; well-known lambda's by var references to them.
  (set! *well-known-lambda-sym-lookup-tbl* candidates)
)


;; Analysis - validation section

;; FUTURE (?): Does given symbol define a procedure?
;(define (avld:procedure? sym) #f)

;; Predicate: Does given symbol refer to a macro?
(define (is-macro? sym)
 (and-let* ((val (env:lookup sym (macro:get-env) #f)))
   (or (tagged-list? 'macro val)
       (Cyc-macro? val))))

;; Does the given function call pass enough arguments?
(define (validate:num-function-args ast)
  ;;(trace:error `(validate:num-function-args ,(car ast) ,ast ,(env:lookup (car ast) (macro:get-env) #f)))
  (and-let* (((app? ast))
             ;; Prims are checked elsewhere
             ((not (prim? (car ast))))
             ((ref? (car ast)))
             ;; Do not validate macros
             ((not (is-macro? (car ast))))
             ;; Extract lambda definition
             (var (adb:get/default (car ast) #f))
             (lam* (adbv:assigned-value var))
             ((pair? lam*))
             ;; Assigned value is boxed in a cell, extract it
             (lam (car lam*))
             ((ast:lambda? lam))
             (formals-type (ast:lambda-formals-type lam))
             ((equal? 'args:fixed formals-type)) ;; Could validate fixed-with-varargs, too
             (expected-argc (length (ast:lambda-args lam)))
             (argc (- (length ast) 1)) )
     (when (not (= argc expected-argc))
       (compiler-msg "Compiler Error: ")
       (compiler-msg ast)
       (compiler-error 
        "Expected "
        (number->string expected-argc)
        " arguments to "
        (symbol->string (car ast))
        " but received "
        (number->string argc))) ))

;; Declare a compiler error and quit
;; Preferable to (error) since a stack trace is meaningless here.
;; Ideally want to supplement this with original line number data and such.
(define (compiler-error . strs)
  (display (apply string-append strs) (current-error-port))
  (newline (current-error-port))
  (exit 1))

;; Display a compilation message to the user
(define (compiler-msg . sexp)
  (display (apply sexp->string sexp) (current-error-port))
  (newline (current-error-port)))

;; Convert given scheme expressions to a string, via (display)
;; TODO: move to util module
(define (sexp->string . sexps)
  (let ((p (open-output-string)))
    (for-each
      (lambda (sexp)
        (apply display (cons sexp (list p))))
      sexps)
    (let ((result (get-output-string p)))
      (close-port p)
      result)))

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

(define mutated-loop-vars '())
(define (mark-mutated-loop-var sym)
  ;(write `(DEBUG mark mutated loop var ,sym)) (newline)
  (set! mutated-loop-vars (cons sym mutated-loop-vars)))
(define (mutated-loop-var? sym)
  (member sym mutated-loop-vars))

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
                    `(cell ;,(car formals)
                       ,(if (mutated-loop-var? (car formals))
                            (with-var (car formals) (lambda (var)
                              (adbv:assigned-value var)))
                            (car formals))
                    )))
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
      ((mutated-loop-var? (set!->var exp))
       #f) ;; essentially no-op in generated code
      (else
        `(,(if (member (set!->var exp) globals)
               'set-global!
               'set-cell!) 
          ,(set!->var exp) 
          ,(wrap-mutables (set!->exp exp) globals)))))
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

(cond-expand
  (cyclone
    ; void : -> void
    (define (void) (if #f #t)))
  (else #f))

))
