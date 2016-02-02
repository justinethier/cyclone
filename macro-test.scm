;; A temporary testbed file
(import (scheme base)
        (scheme eval)
        ;(scheme file)
        ;(scheme read)
        (scheme write)
        (scheme cyclone common)
        (scheme cyclone util)
        ;(scheme cyclone cgen)
        (scheme cyclone transforms)
        (scheme cyclone macros)
        ;(scheme cyclone libraries)
        )

(define input-program '(lambda (x) (begin (+ x x) 1 2 3)))

      ;; Load macros for expansion phase
      (let ((macros (filter 
                      (lambda (v) 
                        (macro? (Cyc-get-cvar (cdr v))))
                      (Cyc-global-vars))))
        (set! *defined-macros*
              (append
                macros
                *defined-macros*)))
      (macro:load-env! *defined-macros* (create-environment '() '()))

      ;; Expand macros
      (set! input-program (my-expand input-program (macro:get-env)))
      (write "---------------- after macro expansion:")
      (write input-program) ;pretty-print

(define (my-expand exp env)
  (cond
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((quote? exp)      exp)
;; TODO: need a way of taking a begin here and splicing its contents
;; into the body
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,@(map 
                            ;; TODO: use extend env here?
                            (lambda (expr) (my-expand expr env))
                            (lambda->exp exp))))
    ((define? exp)     (if (define-lambda? exp)
                           (my-expand (define->lambda exp) env)
                          `(define ,(my-expand (define->var exp) env)
                                ,@(my-expand (define->exp exp) env))))
    ((set!? exp)       `(set! ,(my-expand (set!->var exp) env)
                              ,(my-expand (set!->exp exp) env)))
    ((if? exp)         `(if ,(my-expand (if->condition exp) env)
                            ,(my-expand (if->then exp) env)
                            ,(if (if-else? exp)
                                 (my-expand (if->else exp) env)
                                 ;; Insert default value for missing else clause
                                 ;; FUTURE: append the empty (unprinted) value
                                 ;; instead of #f
                                 #f)))
    ((app? exp)
     (cond
     ((symbol? (car exp))
      (let ((val (env:lookup (car exp) env #f)))
        (if (tagged-list? 'macro val)
          (my-expand ; Could expand into another macro
            (macro:expand exp val env)
            env)
          (map
            (lambda (expr) (my-expand expr env))
            exp))))

     (else
       (map 
        (lambda (expr) (my-expand expr env))
        exp))))
    (else
      (error "unknown exp: " exp))))
