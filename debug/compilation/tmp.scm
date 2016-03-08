(import 
  (scheme base)
  (scheme cyclone util)
  (scheme cyclone transforms))

(define (my-cps-convert ast)

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
(trace:error `(cps-list ,asts ,inner))
    (define (body x)
      (cps-list (cdr asts)
                (lambda (new-asts)
                  (inner (cons x new-asts)))))

    (cond ((null? asts)
           (inner '()))
          ((or (const? (car asts))
               (ref? (car asts)))
           (body (car asts)))
          ;; testing, probably won't work if prim calls into a cont
          ;((prim-call? (car asts))
          ; (body (car asts))) ;; TODO: does nothing, not what we want!
          ;; END testing
          (else
           (let ((r (gensym 'r)))
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

(trace:error
  (my-cps-convert
   '((define test (lambda (a$3 b$2 c$1) (write (cons (+ a$3 b$2 c$1) (- a$3 b$2 c$1))))) ((lambda () 0 (test 1 2 3))))
))

(trace:error
  (my-cps-convert
   '((define test (lambda (a$3 b$2 c$1) (write 
    (cons
      ((lambda (x y z) (list x y z)) 1 2 3)
      (cons (+ a$3 b$2 c$1) (- a$3 b$2 c$1))))) ((lambda () 0 (test 1 2 3))))
)))
