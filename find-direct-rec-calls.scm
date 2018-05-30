(import
  (scheme base)
  (scheme cyclone ast)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  (scheme write)
  ;(srfi 2)
)

  (define (find-direct-recursive-calls exp)
    (define (scan exp ?args?)
      ;;(cond
      ;; ((ast:lambda? exp)
      ;;  (let* ((id (ast:lambda-id exp))
      ;;         (has-cont (ast:lambda-has-cont exp))
      ;;         (sym (string->symbol 
      ;;                (string-append
      ;;                   "lambda-"
      ;;                   (number->string id)
      ;;                   (if has-cont "-cont" ""))))
      ;;         (args* (ast:lambda-args exp))
      ;;         (args (if (null? args*) 
      ;;                   '() 
      ;;                   (formals->list args*)))
      ;;        )
      ;;    (when lp
      ;;      (for-each
      ;;        (lambda (a)
      ;;          (write `(,a defined in a loop))
      ;;          (newline))
      ;;        args)
      ;;    )
      ;;    `(,sym ,(ast:lambda-args exp)
      ;;       ,@(map (lambda (e) (scan e lp)) (ast:lambda-body exp))))
      ;; )
      ;; ((quote? exp) exp)
      ;; ((const? exp) exp)
      ;; ((ref? exp) 
      ;;  (when lp
      ;;      (write `(found variable ,exp within a loop))
      ;;      (newline))
      ;;  exp)
      ;; ((define? exp)
      ;;  `(define ,(define->var exp)
      ;;           ,@(scan (define->exp exp) lp)))
      ;; ((set!? exp)
      ;;  `(set! ,(set!->var exp)
      ;;         ,(scan (set!->exp exp) lp)))
      ;; ((if? exp)       
      ;;  `(if ,(scan (if->condition exp) lp)
      ;;       ,(scan (if->then exp) lp)
      ;;       ,(scan (if->else exp) lp)))
      ;; ((app? exp)
      ;;  (cond
      ;;    ((and-let* ( 
      ;;       ;; Find lambda with initial #f assignment
      ;;       ((ast:lambda? (car exp)))
      ;;       ((pair? (cdr exp)))
      ;;       ((not (cadr exp)))
      ;;       (= 1 (length (ast:lambda-args (car exp))))
      ;;       ;; Get information for continuation
      ;;       (loop-sym (car (ast:lambda-args (car exp))))
      ;;       (inner-exp (car (ast:lambda-body (car exp))))
      ;;       ((app? inner-exp))
      ;;       ((ast:lambda? (car inner-exp)))
      ;;       ;; Find the set (assumes CPS conversion)
      ;;       ((pair? (cdr inner-exp)))
      ;;       ((set!? (cadr inner-exp)))
      ;;       ((equal? (set!->var (cadr inner-exp)) loop-sym))
      ;;       ;; Check the set's continuation
      ;;       ((app? (car (ast:lambda-body (car inner-exp)))))
      ;;       ((equal? (caar (ast:lambda-body (car inner-exp))) loop-sym))
      ;;      )
      ;;     (write `(found named lambda loop ,loop-sym))
      ;;     ;; Continue scanning
      ;;     (map (lambda (e) (scan e #t)) exp)
      ;;    ))
      ;;    (else
      ;;      (map (lambda (e) (scan e lp)) exp))))
      ;; (else exp)))
    )
    (scan exp #f)
  )

;; TEST code:
(define sexp '(
 (define l18 #f)
 (define l12 #f)
 (define l6 #f)
 (define mas
   (lambda-136-cont
     (k$247 x$4$135 y$3$134 z$2$133)
     (shorterp
       (lambda-135
         (r$248)
         (if r$248
           (mas (lambda-133
                  (r$249)
                  (mas (lambda-131
                         (r$250)
                         (mas (lambda-129
                                (r$251)
                                (mas k$247 r$249 r$250 r$251))
                              (cdr z$2$133)
                              x$4$135
                              y$3$134))
                       (cdr y$3$134)
                       z$2$133
                       x$4$135))
                (cdr x$4$135)
                y$3$134
                z$2$133)
           (k$247 z$2$133)))
       y$3$134
       x$4$135)))
 (define shorterp
   (lambda-128-cont
     (k$240 x$6$131 y$5$130)
     (if (null? y$5$130)
       (k$240 #f)
       (if (null? x$6$131)
         (k$240 (null? x$6$131))
         (shorterp k$240 (cdr x$6$131) (cdr y$5$130))))))
))

(find-direct-recursive-calls
  (ast:sexp->ast sexp))
