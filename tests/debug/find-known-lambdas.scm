(import
  (scheme base)
  (scheme cyclone ast)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  (scheme cyclone transforms)
  (scheme write)
  (srfi 2)
  (srfi 69)
)

(define *well-known-lambda-sym-lookup-tbl* #f)

;; Does the given symbol refer to a well-known lambda?
(define (well-known-lambda? sym)
  (and *well-known-lambda-sym-lookup-tbl*
       (hash-table-ref/default *well-known-lambda-sym-lookup-tbl* sym #f)))

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

  ;; Add given lambda to candidate table
  ;; ast:lam - AST Lambda object
  ;; param-sym - Symbol of the parameter that the lambda is passed as
  (define (add-candidate! ast:lam param-sym)
    (hash-table-set! candidates param-sym ast:lam))

  ;; Remove given lambda from candidate table
  ;; param-sym - Symbol representing the lambda to remove
  (define (remove-candidate param-sym)
    (hash-table-delete! candidates param-sym))

  (define (found exp)
    ;; TODO: turn into a trace, update corresponding adbf in analysis DB
    (write `(found known lambda with id ,(ast:lambda-id exp)))
    (newline))

  (define (scan exp)
    (cond
     ((ast:lambda? exp)
      (for-each
        (lambda (e)
          (scan e))
        (ast:lambda-body exp)))
     ((quote? exp) exp)
     ((const? exp) exp)
     ((ref? exp) 
      (remove-candidate exp)
      exp)
     ((define? exp) 
      (for-each
        (lambda (e)
          (scan e))
        (define->exp exp)))
     ;((set!? exp) #f) ;; TODO ??
     ((if? exp)       
      (scan (if->condition exp))
      (scan (if->then exp))
      (scan (if->else exp)))
     ((app? exp)
      (cond
        ((ast:lambda? (car exp))
         (found (car exp))
         (let ((formals (ast:lambda-formals->list (car exp))))
           (when (and (pair? formals)
                      (pair? (cdr exp))
                      (ast:lambda? (cadr exp)))
             (add-candidate! (cadr exp) (car formals)))
         )
         ;; Scan the rest of the args
         (for-each
           (lambda (e)
             (scan e))
           exp))
        (else 
          (for-each
            (lambda (e)
              (scan e))
            ;; Allow candidates to remain if they are just function calls
            (if (ref? (car exp))
                (cdr exp)
                exp)))))
     (else #f)))

;(trace:error `(update-lambda-atv! ,syms ,value))
  (scan exp)
  (for-each
    (lambda (sym/lamb)
      (found (cdr sym/lamb)))
    (hash-table->alist candidates))
  (newline)
  ;; Save the candidate list so we can use it to lookup
  ;; well-known lambda's by var references to them.
  (set! *well-known-lambda-sym-lookup-tbl* candidates)
)

;; TEST code:

;TODO: this is no good, would need to strip out lamdba AST info. maybe we can print it out or something
;so we can correlate everything back
(define sexp '(
; (define my-iota
;   (lambda-11-cont
;     (k$361 n$1$243)
;     ((lambda-9
;        (n$5$244 list$6$245)
;        ((lambda-8
;           (lp$2$7$246)
;           ((lambda-389
;              (lp$2$7$246)
;              ((lambda-1
;                 (r$363)
;                 ((cell-get lp$2$7$246) k$361 n$5$244 list$6$245))
;               (set-cell!
;                 lp$2$7$246
;                 (lambda-7-cont
;                   (k$365 n$8$247 list$9$248)
;                   (if (zero?__inline__ n$8$247)
;                     (k$365 list$9$248)
;                     ((cell-get lp$2$7$246)
;                      k$365
;                      (Cyc-fast-sub n$8$247 1)
;                      (cons (Cyc-fast-sub n$8$247 1) list$9$248)))))))
;            (cell lp$2$7$246)))
;         #f))
;      n$1$243
;      '())))
 (define *size* 511)
 (define classmax 3)
 (define typemax 12)
 (define *iii* 0)
 (define *kount* 0)
 (define *d* 8)
 (define *piececount* #f)
 (define *class* #f)
 (define *piecemax* #f)
 (define *puzzle* #f)
 (define *p* #f)
 (define fit
   (lambda
     (k$394 i$10$249 j$11$250)
     ((lambda
        (end$12$251)
        ((lambda
           (lp$13$17$253)
           ((lambda
              (lp$13$17$253)
              ((lambda
                 (r$396)
                 ((cell-get lp$13$17$253) k$394 0))
               (set-cell!
                 lp$13$17$253
                 (lambda
                   (k$398 k$18$254)
                   ((lambda
                      (k$402)
                      (if (Cyc-fast-gt k$18$254 end$12$251)
                        (k$402 (Cyc-fast-gt k$18$254 end$12$251))
                        (if (vector-ref (vector-ref *p* i$10$249) k$18$254)
                          (k$402 (vector-ref
                                   *puzzle*
                                   (Cyc-fast-plus j$11$250 k$18$254)))
                          (k$402 #f))))
                    (lambda
                      (r$399)
                      (if r$399
                        (k$398 (Cyc-fast-gt k$18$254 end$12$251))
                        ((cell-get lp$13$17$253)
                         k$398
                         (Cyc-fast-plus k$18$254 1)))))))))
            (cell lp$13$17$253)))
         #f))
      (vector-ref *piecemax* i$10$249))))
))

(let ((ast-sexp (ast:sexp->ast sexp)))
  (pretty-print (ast:ast->pp-sexp ast-sexp))
  (analyze:find-known-lambdas ast-sexp))
;    (ast:sexp->ast sexp))
