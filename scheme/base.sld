(define-library (scheme base)
  (export
  )
  (include "cyclone.scm")
  (begin
    (define *Cyc-version-banner* *version-banner*)
    ;; TODO: The whitespace characters are space, tab, line feed, form feed (not in parser yet), and carriage return.
    (define call-with-current-continuation call/cc)
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
    ; TODO: char-ci predicates
    (define (char-upcase c) ;; ASCII-only
      (if (char-lower-case? c)
        (integer->char
          (- (char->integer c)
              (- (char->integer #\a)
                 (char->integer #\A))))
        c))
    (define (char-downcase c) ;; ASCII-only
      (if (char-upper-case? c)
        (integer->char
          (+ (char->integer c)
              (- (char->integer #\a)
                 (char->integer #\A))))
        c))
    ; TODO: char-foldcase
    (define (char-alphabetic? c) (and (char>=? c #\A) (char<=? c #\z))) ;; ASCII-only
    (define (char-upper-case? c) (and (char>=? c #\A) (char<=? c #\Z))) ;; ASCII-only
    (define (char-lower-case? c) (and (char>=? c #\a) (char<=? c #\z))) ;; ASCII-only
    (define (char-numeric? c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (define (char-whitespace? c) (member c '(#\tab #\space #\return #\newline)))
    (define (digit-value c)
      (if (char-numeric? c)
          (- (char->integer c) (char->integer #\0))
          #f))
    (define (foldl func accum lst)
      (if (null? lst)
        accum
        (foldl func (func (car lst) accum) (cdr lst))))
    (define (foldr func end lst)
      (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))
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
    (define (list-tail lst k) 
      (if (zero? k)
        lst
        (list-tail (cdr lst) (- k 1))))
    (define (list-ref lst k)  (car (list-tail lst k)))
    (define (list-set! lst k obj)
      (let ((kth (list-tail lst k)))
        (set-car! kth obj)))
    (define (reverse lst)   (foldl cons '() lst))
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
    (define (make-string k . fill)
      (let ((fill* (if (null? fill)
                      '(#\space)
                      fill)))
        (list->string
          (apply make-list (cons k fill*)))))
    (define (error msg . args)
      (raise (cons msg args)))
    (define (raise obj)
      ((Cyc-current-exception-handler) (list 'raised obj)))
    (define (raise-continuable obj)
      ((Cyc-current-exception-handler) (list 'continuable obj)))
    (define (with-exception-handler handler thunk)
      (let ((result #f)
            (my-handler 
              (lambda (obj)
                (let ((result #f)
                      (continuable? (and (pair? obj) 
                                         (equal? (car obj) 'continuable))))
                  ;; Unregister this handler since it is no longer needed
                  (Cyc-remove-exception-handler)
                  (set! result (handler (cadr obj))) ;; Actual handler
                  (if continuable?
                      result
                      (error "exception handler returned"))))))
      ;; No cond-expand below, since this is part of our internal lib
      (Cyc-add-exception-handler my-handler)
      (set! result (thunk))
      (Cyc-remove-exception-handler) ; Only reached if no ex raised
      result))
    (define *exception-handler-stack* '())
    (define (Cyc-add-exception-handler h) 
       (set! *exception-handler-stack* (cons h *exception-handler-stack*)))
    (define (Cyc-remove-exception-handler)
       (if (not (null? *exception-handler-stack*))
          (set! *exception-handler-stack* (cdr *exception-handler-stack*))))
  ;  (define (Cyc-current-exception-handler)
  ;    (if (null? *exception-handler-stack*)
  ;      Cyc-default-exception-handler
  ;      (car *exception-handler-stack*)))
))
