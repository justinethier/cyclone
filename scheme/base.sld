(define-library (scheme base)
  (export
    ; TODO: need filter for the next two. also, they really belong in SRFI-1, not here
    ;delete
    ;delete-duplicates
    call-with-current-continuation
    call/cc
    call-with-values
    dynamic-wind
    values
    ;(Cyc-bin-op cmp x lst)
    ;(Cyc-bin-op-char cmp c cs)
    char=?
    char<?
    char>?
    char<=?
    char>=?
    string=?
    string<?
    string<=?
    string>?
    string>=?
    foldl
    foldr
    not
    list?
    zero?
    positive?
    negative?
    append
    list
    make-list
    list-copy
    map
    for-each
    list-tail
    list-ref
    list-set!
    reverse
    boolean=?
    symbol=?
    Cyc-obj=?
    vector
    vector-append
    vector-copy
    vector-copy!
    vector-fill!
    vector->list
    vector->string
    vector-map
    vector-for-each
    make-string
    string
    string-copy
    string-copy!
    string-fill!
    string->list
    string->vector
    string-map
    string-for-each
    make-parameter
    current-output-port
    current-input-port
    current-error-port
    call-with-port
    ; TODO: error-object?
    ; TODO: error-object-message
    ; TODO: error-object-irritants
    ; TODO: file-error?
    ; TODO: read-error?
    error
    raise
    raise-continuable
    with-exception-handler
    *exception-handler-stack*
    Cyc-add-exception-handler
    Cyc-remove-exception-handler
    newline
    write-char
    flush-output-port
    read-line
    features
    any
    every
    and
    or
    let
    let*
    letrec
    begin
    case
    cond
    cond-expand
    when
    quasiquote
  )
  (begin
    ;; Features implemented by this Scheme
    (define (features) '(cyclone r7rs exact-closed))

    (define-syntax and
      (er-macro-transformer
       (lambda (expr rename compare)
         (cond ((null? (cdr expr)) #t)
               ((null? (cddr expr)) (cadr expr))
               (else (list (rename 'if) (cadr expr)
                           (cons (rename 'and) (cddr expr))
                           #f))))))
    (define-syntax or
      (er-macro-transformer
        (lambda (expr rename compare)
          (cond ((null? (cdr expr)) #f)
                ((null? (cddr expr)) (cadr expr))
                (else
                 (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                       (list (rename 'if) (rename 'tmp)
                             (rename 'tmp)
                             (cons (rename 'or) (cddr expr)))))))))
    (define-syntax let
      (er-macro-transformer
        (lambda (expr rename compare)
          (if (null? (cdr expr)) (error "empty let" expr))
          (if (null? (cddr expr)) (error "no let body" expr))
          ((lambda (bindings)
             (if (list? bindings) #f (error "bad let bindings"))
             (if (every (lambda (x)
                          (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                        bindings)
                 ((lambda (vars vals)
                    (if (symbol? (cadr expr))
                        `((,(rename 'lambda) ,vars
                           (,(rename 'letrec) ((,(cadr expr)
                                                (,(rename 'lambda) ,vars
                                                 ,@(cdr (cddr expr)))))
                            (,(cadr expr) ,@vars)))
                          ,@vals)
                        `((,(rename 'lambda) ,vars ,@(cddr expr)) ,@vals)))
                  (map car bindings)
                  (map cadr bindings))
                 (error "bad let syntax" expr)))
           (if (symbol? (cadr expr)) (car (cddr expr)) (cadr expr))))))
    (define-syntax let*
      (er-macro-transformer
        (lambda (expr rename compare)
          (if (null? (cdr expr)) (error "empty let*" expr))
          (if (null? (cddr expr)) (error "no let* body" expr))
          (if (null? (cadr expr))
              `(,(rename 'let) () ,@(cddr expr))
              (if (if (list? (cadr expr))
                      (every
                       (lambda (x)
                         (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                       (cadr expr))
                      #f)
                  `(,(rename 'let) (,(caar (cdr expr)))
                    (,(rename 'let*) ,(cdar (cdr expr)) ,@(cddr expr)))
                  (error "bad let* syntax"))))))
    (define-syntax letrec 
      (er-macro-transformer
        (lambda (exp rename compare)
          (let* ((bindings  (cadr exp)) ;(letrec->bindings exp)
                 (namings   (map (lambda (b) (list (car b) #f)) bindings))
                 (names     (map car (cadr exp))) ;(letrec->bound-vars exp)
                 (sets      (map (lambda (binding) 
                                   (cons 'set! binding))
                                 bindings))
                 (args      (map cadr (cadr exp)))) ;(letrec->args exp)
            `(let ,namings
               (begin ,@(append sets (cddr exp)))))))) ;(letrec->exp exp)
;; NOTE: chibi uses the following macro. turns vars into defines?
;;(define-syntax letrec
;;  (er-macro-transformer
;;   (lambda (expr rename compare)
;;     ((lambda (defs)
;;        `((,(rename 'lambda) () ,@defs ,@(cddr expr))))
;;      (map (lambda (x) (cons (rename 'define) x)) (cadr expr))))))
    (define-syntax begin 
      (er-macro-transformer
        (lambda (exp rename compare)
          (define (singlet? l)
            (and (list? l)
                 (= (length l) 1)))
          
          (define (dummy-bind exps)
            (cond
              ((singlet? exps)  (car exps))
              
              ; JAE - should be fine until CPS phase
              ((pair? exps)
               `((lambda ()
                 ,@exps)))))
              ;((pair? exps)     `(let (($_ ,(car exps)))
              ;                    ,(dummy-bind (cdr exps))))))
          (dummy-bind (cdr exp)))))
    (define-syntax cond-expand
      (er-macro-transformer
        ;; Based on the cond-expand macro from Chibi scheme
        (lambda (expr rename compare)
          (define (check x)
            (if (pair? x)
                (case (car x)
                  ((and) (every check (cdr x)))
                  ((or) (any check (cdr x)))
                  ((not) (not (check (cadr x))))
                  ;((library) (eval `(find-module ',(cadr x)) (%meta-env)))
                  (else (error "cond-expand: bad feature" x)))
                (memq x (features))))
          (let expand ((ls (cdr expr)))
            (cond ((null? ls))  ; (error "cond-expand: no expansions" expr)
                  ((not (pair? (car ls))) (error "cond-expand: bad clause" (car ls)))
                  ((eq? 'else (caar ls)) ;(identifier->symbol (caar ls)))
                   (if (pair? (cdr ls))
                       (error "cond-expand: else in non-final position")
                       `(,(rename 'begin) ,@(cdar ls))))
                  ((check (caar ls)) `(,(rename 'begin) ,@(cdar ls)))
                  (else (expand (cdr ls))))))))
    (define-syntax cond
      (er-macro-transformer
          (lambda (expr rename compare)
            (if (null? (cdr expr))
                #f ;(if #f #f)
                ((lambda (cl)
                   (if (compare (rename 'else) (car cl))
                       (if (pair? (cddr expr))
                           (error "non-final else in cond" expr)
                           (list (cons (rename 'lambda) (cons '() (cdr cl)))))
                       (if (if (null? (cdr cl)) #t (compare (rename '=>) (cadr cl)))
                           (list (list (rename 'lambda) (list (rename 'tmp))
                                       (list (rename 'if) (rename 'tmp)
                                             (if (null? (cdr cl))
                                                 (rename 'tmp)
                                                 (list (car (cddr cl)) (rename 'tmp)))
                                             (cons (rename 'cond) (cddr expr))))
                                 (car cl))
                           (list (rename 'if)
                                 (car cl)
                                 (list (cons (rename 'lambda) (cons '() (cdr cl))))
                                 (cons (rename 'cond) (cddr expr))))))
                 (cadr expr))))))
    (define-syntax case
      (er-macro-transformer
          (lambda (expr rename compare)
            (define (body exprs)
              (cond
               ((null? exprs)
                (rename 'tmp))
               ((compare (rename '=>) (car exprs))
                `(,(cadr exprs) ,(rename 'tmp)))
               (else
                `(,(rename 'begin) ,@exprs))))
            (define (clause ls)
              (cond
               ((null? ls) #f)
               ((compare (rename 'else) (caar ls))
                (body (cdar ls)))
               ((and (pair? (car (car ls))) (null? (cdr (car (car ls)))))
                `(,(rename 'if) (,(rename 'eqv?) ,(rename 'tmp)
                                 (,(rename 'quote) ,(car (caar ls))))
                  ,(body (cdar ls))
                  ,(clause (cdr ls))))
               (else
                `(,(rename 'if) (,(rename 'memv) ,(rename 'tmp)
                                 (,(rename 'quote) ,(caar ls)))
                  ,(body (cdar ls))
                  ,(clause (cdr ls))))))
            `(let ((,(rename 'tmp) ,(cadr expr)))
               ,(clause (cddr expr))))))
    (define-syntax when
      (er-macro-transformer
        (lambda (exp rename compare)
          (if (null? (cdr exp)) (error "empty when" exp))
          (if (null? (cddr exp)) (error "no when body" exp))
          `(if ,(cadr exp)
               ((lambda () ,@(cddr exp)))
               #f))))
    (define-syntax quasiquote
      (er-macro-transformer
        ;; Based on the quasiquote macro from Chibi scheme
        (lambda (expr rename compare)
          (define (qq x d)
            (cond
             ((pair? x)
              (cond
               ((compare (rename 'unquote) (car x))
                (if (<= d 0)
                    (cadr x)
                    (list (rename 'list) (list (rename 'quote) 'unquote)
                          (qq (cadr x) (- d 1)))))
               ((compare (rename 'unquote-splicing) (car x))
                (if (<= d 0)
                    (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
                    (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
                          (qq (cadr x) (- d 1)))))
               ((compare (rename 'quasiquote) (car x))
                (list (rename 'list) (list (rename 'quote) 'quasiquote)
                      (qq (cadr x) (+ d 1))))
               ((and (<= d 0) (pair? (car x))
                     (compare (rename 'unquote-splicing) (caar x)))
                (if (null? (cdr x))
                    (cadr (car x))
                    (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
               (else
                (list (rename 'cons) (qq (car x) d) (qq (cdr x) d)))))
             ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
             ((if (symbol? x) #t (null? x)) (list (rename 'quote) x))
             (else x)))
          (qq (cadr expr) 0))))

    ;; TODO: The whitespace characters are space, tab, line feed, form feed (not in parser yet), and carriage return.
    (define call-with-current-continuation call/cc)
    ;; TODO: this is from r7rs, but is not really good enough by itself
    (define (values . things)
      (call/cc
        (lambda (cont) (apply cont things))))
    ;; TODO: just need something good enough for bootstrapping (for now)
    ;; does not have to be perfect (this is not, does not handle call/cc or exceptions)
    (define (dynamic-wind before thunk after)
      (before)
      (call-with-values
        thunk
        (lambda (result) ;results
          (after)
          result)))
          ;(apply values results))))
    (define (call-with-port port proc)
      (let ((result (proc port)))
        (close-port port)
        result))
    (define (Cyc-bin-op cmp x lst)
      (cond
        ((null? lst) #t)
        ((cmp x (car lst))
         (Cyc-bin-op cmp (car lst) (cdr lst)))
        (else #f)))
    (define (Cyc-bin-op-char cmp c cs)
      (Cyc-bin-op
        (lambda (x y) 
          (cmp (char->integer x) (char->integer y)))
        c
        cs))
    (define (char=?  c1 c2 . cs) (Cyc-bin-op-char =  c1 (cons c2 cs)))
    (define (char<?  c1 c2 . cs) (Cyc-bin-op-char <  c1 (cons c2 cs)))
    (define (char>?  c1 c2 . cs) (Cyc-bin-op-char >  c1 (cons c2 cs)))
    (define (char<=? c1 c2 . cs) (Cyc-bin-op-char <= c1 (cons c2 cs)))
    (define (char>=? c1 c2 . cs) (Cyc-bin-op-char >= c1 (cons c2 cs)))
    ; TODO: char-ci predicates (in scheme/char library)


    (define (string=? str1 str2)  (equal? (string-cmp str1 str2) 0))
    (define (string<? str1 str2)  (<  (string-cmp str1 str2) 0))
    (define (string<=? str1 str2) (<= (string-cmp str1 str2) 0))
    (define (string>? str1 str2)  (>  (string-cmp str1 str2) 0))
    (define (string>=? str1 str2) (>= (string-cmp str1 str2) 0))
    ; TODO: generalize to multiple arguments: (define (string<? str1 str2 . strs)

    (define (foldl func accum lst)
      (if (null? lst)
        accum
        (foldl func (func (car lst) accum) (cdr lst))))
    (define (foldr func end lst)
      (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))
    (define (read-line . port)
      (if (null? port)
        (Cyc-read-line (current-output-port))
        (Cyc-read-line (car port))))
    (define (flush-output-port . port)
      (if (null? port)
        (Cyc-flush-output-port (current-output-port))
        (Cyc-flush-output-port (car port))))
    (define (write-char char . port)
      (if (null? port)
        (Cyc-write-char char (current-output-port))
        (Cyc-write-char char (car port))))
    (define (newline . port) 
      (apply write-char (cons #\newline port)))
    (define (not x) (if x #f #t))
    (define (list? o)
      (define (_list? obj)
        (cond
          ((null? obj) #t)
          ((pair? obj)
           (_list? (cdr obj)))
          (else #f)))
      (if (Cyc-has-cycle? o)
        #t
        (_list? o)))
    (define (zero? n) (= n 0))
    (define (positive? n) (> n 0))
    (define (negative? n) (< n 0))
    ; append accepts a variable number of arguments, per R5RS. So a wrapper
    ; has been provided for the standard 2-argument version of (append).
    ;
    ; We return the given value if less than 2 arguments are given, and
    ; otherwise fold over each arg, appending it to its predecessor. 
    (define (append . lst)
      (define append-2
              (lambda (inlist alist)
                      (foldr (lambda (ap in) (cons ap in)) alist inlist)))
      (if (null? lst)
          lst
          (if (null? (cdr lst))
              (car lst)
              (foldl (lambda (a b) (append-2 b a)) (car lst) (cdr lst)))))
    (define (list . objs)  objs)
    (define (make-list k . fill)
      (letrec ((x (if (null? fill) 
                   #f
                   (car fill)))
               (make
                 (lambda (n obj)
                   (if (zero? n)
                   '()
                   (cons obj (make (- n 1) obj) )))))
      (make k x)))
    (define (list-copy lst)
      (foldr (lambda (x y) (cons x y)) '() lst))
    (define (map func lst)
      (foldr (lambda (x y) (cons (func x) y)) '() lst))
    (define (for-each f lst)
      (cond
       ((null? lst) #t)
       (else
         (f (car lst))
         (for-each f (cdr lst)))))
; TODO:
;(define (vector-map fnc . vargs)
;    (let ((ls (map vector->list v vargs)))
;        (list->vector
;            (apply map
;                   (cons fnc ls)))))
;
;(define (vector-for-each fnc . vargs)
;    (let ((ls (map vector->list vargs)))
;        (apply for-each
;               (cons fnc ls))))
    (define (list-tail lst k) 
      (if (zero? k)
        lst
        (list-tail (cdr lst) (- k 1))))
    (define (list-ref lst k)  (car (list-tail lst k)))
    (define (list-set! lst k obj)
      (let ((kth (list-tail lst k)))
        (set-car! kth obj)))
    (define (reverse lst)   (foldl cons '() lst))

    (define (vector . objs) (list->vector objs))
    (define (vector->list vec . opts)
      (letrec ((len (vector-length vec))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i lst)
                       (if (= i end)
                           (reverse lst)
                           (loop (+ i 1) 
                                 (cons (vector-ref vec i) lst))))))
        (loop start '())))
    (define (vector->string vec . opts)
      (let ((lst (apply vector->list (cons vec opts))))
        (list->string lst)))
    (define (string->list str . opts)
      (letrec ((len (string-length str))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i lst)
                       (if (= i end)
                           (reverse lst)
                           (loop (+ i 1) 
                                 (cons (string-ref str i) lst))))))
        (loop start '())))
    ;; TODO: need to extend string->list to take optional start/end args, 
    ;; then modify this function to work with optional args, too
    (define (string->vector str . opts)
      (list->vector
        (string->list str)))
    (define (string-copy str . opts)
      (letrec ((len (string-length str))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len)))
        (substring str start end)))
    (define (string-copy! to at from . opts)
      (letrec ((len (string-length from))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i-at i-from)
                       (cond
                        ((= i-from end) to)
                        (else
                          (string-set! to i-at (string-ref from i-from))
                          (loop (+ i-at 1) (+ i-from 1)))))))
        (loop at start)))
    (define (string-fill! str fill . opts)
      (letrec ((len (string-length str))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i)
                       (cond
                        ((= i end) str)
                        (else
                          (string-set! str i fill)
                          (loop (+ i 1)))))))
        (loop start)))
    (define (string-map func str)
      (list->string (map func (string->list str))))
    (define (string-for-each func str)
      (for-each func (string->list str)))
    (define (vector-map func vec)
      (list->vector (map func (vector->list vec)))) 
    (define (vector-for-each func vec)
      (for-each func (vector->list vec)))
    (define (vector-append . vecs)
      (list->vector
        (apply append (map vector->list vecs))))
    (define (vector-copy vec . opts)
      (letrec ((len (vector-length vec))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i new-vec)
                       (cond
                        ((= i end)
                         new-vec)
                        (else
                           (vector-set! new-vec i (vector-ref vec i))
                           (loop (+ i 1) new-vec))))))
        (loop start (make-vector (- end start) #f))))
    ;; TODO: does not quite meet r7rs spec, should check if vectors overlap
    (define (vector-copy! to at from . opts)
      (letrec ((len (vector-length from))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i-at i-from)
                       (cond
                        ((= i-from end) to)
                        (else
                          (vector-set! to i-at (vector-ref from i-from))
                          (loop (+ i-at 1) (+ i-from 1)))))))
        (loop at start)))
    ;; TODO: this len/start/end/loop pattern is common, could use a macro for it
    (define (vector-fill! vec fill . opts)
      (letrec ((len (vector-length vec))
               (start (if (> (length opts) 0) (car opts) 0))
               (end (if (> (length opts) 1) (cadr opts) len))
               (loop (lambda (i)
                       (cond
                        ((= i end) vec)
                        (else
                          (vector-set! vec i fill)
                          (loop (+ i 1)))))))
        (loop start)))

    (define (boolean=? b1 b2 . bs)
      (Cyc-obj=? boolean? b1 (cons b2 bs)))
    (define (symbol=? sym1 sym2 . syms)
      (Cyc-obj=? symbol? sym1 (cons sym2 syms)))
    (define (Cyc-obj=? type? obj objs)
      (and
        (type? obj)
        (call/cc
          (lambda (return)
            (for-each 
              (lambda (o)
                (if (not (eq? o obj))
                  (return #f)))
              objs)
              #t))))
    (define (string . chars)
      (list->string chars))
    (define (make-string k . fill)
      (let ((fill* (if (null? fill)
                      '(#\space)
                      fill)))
        (list->string
          (apply make-list (cons k fill*)))))
    (define (make-parameter init . o)
      (let* ((converter
               (if (pair? o) (car o) (lambda (x) x)))
             (value (converter init)))
        (lambda args
          (cond
            ((null? args)
             value)
            ((eq? (car args) '<param-set!>)
             (set! value (cadr args)))
            ((eq? (car args) '<param-convert>)
             converter)
           (else
             (error "bad parameter syntax"))))))
    (define current-output-port
      (make-parameter (Cyc-stdout)))
    (define current-input-port
      (make-parameter (Cyc-stdin)))
    (define current-error-port
      (make-parameter (Cyc-stderr)))
    (define (error msg . args)
      (raise (cons msg args)))
    (define (raise obj)
      ((Cyc-current-exception-handler) 
        (cons 'raised (if (pair? obj) obj (list obj)))))
    (define (raise-continuable obj)
      ((Cyc-current-exception-handler) 
        (cons 'continuable (if (pair? obj) obj (list obj)))))
    (define (with-exception-handler handler thunk)
      (let ((result #f)
            (my-handler 
              (lambda (obj)
                (let ((result #f)
                      (continuable? (and (pair? obj) 
                                         (equal? (car obj) 'continuable))))
                  ;; Unregister this handler since it is no longer needed
                  (Cyc-remove-exception-handler)
                  (set! result (handler (cdr obj))) ;; Actual handler
                  (if continuable?
                      result
                      (error "exception handler returned"))))))
      ;; No cond-expand below, since this is part of our internal lib
      (Cyc-add-exception-handler my-handler)
      (set! result (thunk))
      (Cyc-remove-exception-handler) ; Only reached if no ex raised
      result))
    (define *exception-handler-stack* '())
;    (define (Cyc-add-exception-handler h) 
;       (set! *exception-handler-stack* (cons h *exception-handler-stack*)))
;    (define (Cyc-remove-exception-handler)
;       (if (not (null? *exception-handler-stack*))
;          (set! *exception-handler-stack* (cdr *exception-handler-stack*))))
    (define-c Cyc-add-exception-handler
      "(void *data, int argc, closure _, object k, object h)"
      " gc_thread_data *thd = (gc_thread_data *)data;
        make_cons(c, h, thd->exception_handler_stack);
        thd->exception_handler_stack = &c;
        return_closcall1(data, k, &c); ")
    (define-c Cyc-remove-exception-handler
      "(void *data, int argc, closure _, object k)"
      " gc_thread_data *thd = (gc_thread_data *)data;
        if (thd->exception_handler_stack) {
          thd->exception_handler_stack = cdr(thd->exception_handler_stack);
        }
        return_closcall1(data, k, thd->exception_handler_stack); ")

  ;; Simplified versions of every/any from SRFI-1
  (define (any pred lst)
    (let any* ((l (map pred lst)))
        (cond
          ((null? l) #f) ; Empty list
          ((car l)   #t) ; Done
          (else 
             (any* (cdr l))))))
  (define (every pred lst)
    (let every* ((l (map pred lst)))
        (cond
          ((null? l) #t) ; Empty list
          ((car l)
             (every* (cdr l)))
          (else 
             #f))))

))
