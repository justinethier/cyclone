;;
;; Cyclone Scheme
;; Copyright (c) 2015, Justin Ethier
;; All rights reserved.
;;
;; This module contains various utility functions.
;;
(define-library (scheme cyclone util)
  (import (scheme base)
          (scheme char))
  ; TODO: really need export-all for these cyclone libs!!
  (export
    ;; Code analysis
    tagged-list?
    if?
    begin?
    lambda?
    ;; Environments
    env:enclosing-environment
    env:first-frame
    env:the-empty-environment
    env:make-frame
    env:frame-variables
    env:frame-values 
    env:add-binding-to-frame! 
    env:extend-environment 
    env:lookup
    env:lookup-variable-value 
    env:_lookup-variable-value 
    env:set-variable-value! 
    env:define-variable! 
    ;; ER macro supporting functions
    Cyc-er-rename
    Cyc-er-compare?
    ;; Code generation
    mangle
    mangle-global
    ;; Scheme library functions
    gensym
    delete
    delete-duplicates
    list-insert-at!
    filter)
  (begin

(define (tagged-list? tag exp)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

; begin? : exp -> boolean
(define (begin? exp) 
  (tagged-list? 'begin exp))

; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))

;; Simplified version of filter from SRFI 1
(define (filter pred lis)
  (letrec ((recur (lambda (lis)
                    (if (null? lis)
                     lis
                     (let ((head (car lis))
                           (tail (cdr lis)))
                       (if (pred head)
                           (let ((new-tail (recur tail)))
                         (if (eq? tail new-tail) lis
                             (cons head new-tail)))
                           (recur tail)))))))
    (recur lis)))

;; Based off corresponding SRFI-1 definition
(define (delete x lis)
  (filter (lambda (y) (not (equal? x y))) lis))

;; Inefficient version based off code from SRFI-1
(define (delete-duplicates lis)
  (define (recur lis) ; ((lis lis))
    (if (null? lis) lis
        (let* ((x (car lis))
               (tail (cdr lis))
               (new-tail (recur (delete x tail))))
          (if (eq? tail new-tail) lis (cons x new-tail)))))
  (recur lis))

;; Insert obj at index k of list, increasing length of list by one.
(define (list-insert-at! lis obj k)
  (cond
   ((null? lis) (error "list-insert-at!, lis cannot be null"))
   ((and (> k 0) (null? (cdr lis)))
    (set-cdr! lis (cons obj '())))
   ((zero? k)
    (let ((old-car (car lis)))
      (set-car! lis obj)
      (set-cdr! lis (cons old-car (cdr lis)))))
   (else
    (list-insert-at! (cdr lis) obj (- k 1)))))

; gensym-count : integer
(define gensym-count 0)

; gensym : symbol -> symbol
(define gensym (lambda params
                 (if (null? params)
                     (begin
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append
                                        "$"
                                        (number->string gensym-count))))
                     (begin
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append 
                                        (if (symbol? (car params))
                                            (symbol->string (car params))
                                            (car params))
                                        "$"
                                        (number->string gensym-count)))))))

;;;; Environments
;;;; TODO: longer-term, move these into their own module
(define (env:enclosing-environment env) (cdr env))
(define (env:first-frame env) (car env))
(define env:the-empty-environment '())

(define (env:make-frame variables values)
  (cons variables values))
(define (env:frame-variables frame) (car frame))
(define (env:frame-values frame) (cdr frame))
(define (env:add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (env:extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (env:make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (env:lookup-variable-value var env)
  (env:_lookup-variable-value var env 
    (lambda ()
      (error "Unbound variable" var))))

(define (env:_lookup-variable-value var env not-found)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (env:enclosing-environment env)))
            ((eq? var (car vars))
             (cond-expand
               (cyclone
                 (Cyc-get-cvar (car vals)))
               (else 
                 (car vals))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env env:the-empty-environment)
        (not-found)
        (let ((frame (env:first-frame env)))
          (scan (env:frame-variables frame)
                (env:frame-values frame)))))
  (env-loop env))

(define (env:lookup var env default-value)
  (env:_lookup-variable-value var env (lambda () default-value)))

(define (env:set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (env:enclosing-environment env)))
            ((eq? var (car vars))
             (cond-expand
               (cyclone
                 (if (Cyc-cvar? (car vals))
                   (Cyc-set-cvar! (car vals) val)
                   (set-car! vals val)))
               (else
                 (set-car! vals val))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env env:the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (env:first-frame env)))
          (scan (env:frame-variables frame)
                (env:frame-values frame)))))
  (env-loop env))

(define (env:define-variable! var val env)
  (let ((frame (env:first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (env:add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             ;; TODO: update compiled var
             ;; cond-expand
             ;;  if cvar
             ;;     set-cvar
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (env:frame-variables frame)
          (env:frame-values frame))))
;;;; END Environments


;;; Explicit renaming macros

;; ER macro rename function, based on code from Chibi scheme
(define Cyc-er-rename
  (lambda (sym) sym)) ; TODO: temporary placeholder, see below
; Notes:
;
; need to figure out what to return from this function so that renaming
; actually does what it is supposed to do (or a close approximation).
; then need to figure out what needs to change in the rest of the code to
; support that.
; 
; how renaming should work:
;
;  - ideally, add a closure from the macro-env for identifier
;  - practically, if identifier is defined in mac-env, gensym but
;    update mac-env so renamed variable points to original.
;    if not defined, is it the same as a gensym? or nothing at all???
;
;in order for this to work:
;
; - compiler needs to maintain env consisting of at least macros,
;   and pass this along. presumably this env would be used instead of
;   *defined-macros*.
; - interpreter can use a-env and global-env??????
;   there are open questions about extending a-env, but without eval being
;   able to define-syntax (yet), I think we can defer that until later.
; - environment code needs to be added to a common place, away from eval.sld
;
;   can pass mac-env, useenv in to this guy (and compare as well), and possibly add renamed bindings to it.
;
; mac-env is 
;  - global env for interpreted macros, at least for now until
;    they can be recognized by eval
;  - ?? for compiled macros
;
; use-env is:
;  - current env for eval, can be passed in.
;    is this really a-env though? or do we need to extend it when
;    a new lambda scope is introduced?
;  - need to keep track of it for compiled macro expansion
;
;  ((lambda (renames)
;     (lambda (identifier)
;       ((lambda (cell)
;          (if cell
;              (cdr cell)
;              ((lambda (name)
;                 (set! renames (cons (cons identifier name) renames))
;                 name)
;               (gensym identifier)
;               ; gensym not good enough, need to also preserve ref trans.
;               ; also note that an identifier can be an object, it does not
;               ; just have to be a symbol. although, of course, the rest
;               ; of the code needs to be able to handle identifiers in
;               ; forms other than symbols, if that is done.
;               ;
;               ;(make-syntactic-closure mac-env '() identifier)
;              )))
;        (assq identifier renames))))
;   ;; TODO: For now, do not allow renaming of special form symbols to 
;   ;; prevent issues within the compiler
;   '(
;     (define . define)
;     (define-syntax . define-syntax)
;     (if . if)
;     (lambda . lambda)
;     (quote . quote)
;     (set! . set!)
;    )))

(define (Cyc-er-compare? a b)
  ;; TODO: this is not good enough, need to determine if these symbols
  ;; are the same identifier in their *environment of use*
  (eq? a b))

;; Name-mangling.

;; We have to "mangle" Scheme identifiers into
;; C-compatible identifiers, because names like
;; foo-bar/baz are not identifiers in C.

; mangle : symbol -> string
(define (mangle symbol)
 (letrec
   ((m (lambda (chars)
      (if (null? chars)
        '()
        (if (or (and (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)))
                (char-numeric? (car chars)))
            (cons (car chars) (m (cdr chars)))
            (cons #\_ (append (integer->char-list (char->natural (car chars)))
                              (m (cdr chars))))))))
    (ident (list->string (m (string->list (symbol->string symbol))))))
   (if (member (string->symbol ident) *c-keywords*)
     (string-append "_" ident)
     ident)))

(define (mangle-global symbol)
  (string-append "__glo_" (mangle symbol)))

(define *c-keywords* 
 '(auto _Bool break case char _Complex const continue default do double else
   enum extern float for goto if _Imaginary inline int long register restrict
   return short signed sizeof static struct switch typedef union unsigned
   void volatile while
   list  ;; Not a keyword but reserved type
   ))
;; END name mangling section

    ))
