;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains the s-expression parser and supporting functions.
;;
(define-library (scheme read)
  (import (scheme base)
          (scheme char))
  (export
    read
    read-all
  )
  (begin

(define read cyc-read)

;; Extended information for each input port
(define *in-port-table* '())
(define (reg-port fp)
  (let ((r (assoc fp *in-port-table*)))
    (cond
     ((not r)
;(write `(ADDED NEW ENTRY TO in port table!!))
      (set! r 
        (list fp 
              #f  ; Buffered char, if any
              1   ; Line number
              0)) ; Char number
      (set! *in-port-table* (cons r *in-port-table*))
      r)
     (else r))))
;; TODO: unreg-port - delete fp entry from *in-port-table*
;; would want to do this when port is closed

(define (in-port:read-buf! ptbl)
 (let ((result (cadr ptbl)))
   (in-port:set-buf! ptbl #f)
   result))
(define (in-port:get-buf ptbl) (cadr ptbl))
(define (in-port:set-buf! ptbl buf) (set-car! (cdr ptbl) buf))
(define (in-port:get-lnum ptbl) (caddr ptbl))
(define (in-port:set-lnum! ptbl lnum) (set-car! (cddr ptbl) lnum))
(define (in-port:get-cnum ptbl) (cadddr ptbl))
(define (in-port:set-cnum! ptbl cnum) (set-car! (cdddr ptbl) cnum))
;; END input port table

;; Helper functions
(define (add-tok tok toks)
  (cons tok toks))

;; Get completed list of tokens
(define (get-toks tok toks)
  (if (null? tok)
    toks
    (add-tok (->tok tok) toks)))

;; Add a token to the list, quoting it if necessary
(define (->tok lst)
  (parse-atom (reverse lst)))

;; Did we read a dotted list
(define (dotted? lst) 
  (and (> (length lst) 2)
       (equal? (cadr (reverse lst)) (string->symbol "."))))

;; Convert a list read by the reader into an improper list
(define (->dotted-list lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) (string->symbol "."))
     (cadr lst))
    (else
      (cons (car lst) (->dotted-list (cdr lst))))))

(define (parse-error msg lnum cnum)
  (error
    (string-append
      "Error (line "
      (number->string lnum)
      ", char "
      (number->string cnum)
      "): "
      msg)))

;; Add finished token, if there is one, and continue parsing
(define (parse/tok fp tok toks all? comment? parens ptbl curr-char)
  (cond
   ((null? tok)
    (parse fp '() toks all? comment? parens ptbl))
   (all?
    (parse fp '() 
           (add-tok (->tok tok) toks) 
           all?
           comment? 
           parens
           ptbl))
   (else
     ;; Reached a terminating char, return current token and
     ;; save term char for the next (read).
     ;; Note: never call set-buf! if in "all?" mode, since
     ;;       that mode builds a list of tokens
     (in-port:set-buf! ptbl curr-char)
;(write `(DEBUG ,tok ,ptbl))
;(write "\n")
     (car (add-tok (->tok tok) toks)))))

;; Parse input from stream
;;
;; Input:
;; - Port object
;; - Current token
;; - List of tokens read (if applicable)
;; - Bool - Read-all mode, or just read the next object?
;; - Bool - Are we inside a comment?
;; - Level of nested parentheses 
;; - Entry in the in-port table for this port
;;
;; Output: next object, or list of objects (if read-all mode)
;; 
(define (parse fp tok toks all? comment? parens ptbl)
  (in-port:set-cnum! ptbl 
    (+ 1 (in-port:get-cnum ptbl)))

  (let ((c (if (in-port:get-buf ptbl)
               (in-port:read-buf! ptbl) ;; Already buffered
               (read-char fp))))
;; DEBUGGING
;(write `(DEBUG read ,tok ,c))
;(write (newline))
;; END DEBUG
    (cond
      ((eof-object? c) 
       (if (> parens 0)
           (parse-error "missing closing parenthesis" 
             (in-port:get-lnum ptbl)
             (in-port:get-cnum ptbl)))
       (if all?
         (reverse (get-toks tok toks))
         (let ((last (get-toks tok toks)))
           (if (> (length last) 0)
             (car last)
             c)))) ;; EOF
      (comment?
       (if (eq? c #\newline)
           (begin
              (in-port:set-lnum! ptbl 
                (+ 1 (in-port:get-lnum ptbl)))
              (in-port:set-cnum! ptbl 0)
              (parse fp '() toks all? #f parens ptbl))
           (parse fp '() toks all? #t parens ptbl)))
      ((char-whitespace? c)
       (if (equal? c #\newline)
           (in-port:set-lnum! ptbl 
             (+ 1 (in-port:get-lnum ptbl))))
       (if (equal? c #\newline) 
           (in-port:set-cnum! ptbl 0))
       (parse/tok fp tok toks all? #f parens ptbl c))
      ((eq? c #\;)
       (parse/tok fp tok toks all? #t parens ptbl c))
      ((eq? c #\')
       (cond
         ((and (not all?) (not (null? tok)))
           ;; Reached a terminal char, read out previous token
;; TODO: would also need to do this if previous char was
;;       not a quote!
;;       EG: 'a'b ==> (quote a) (quote b), NOT (quote (quote b))
          (in-port:set-buf! ptbl c)
          (car (add-tok (->tok tok) toks)))
         (else
           ;; Read the next expression and wrap it in a quote
           (let ((sub
                   (parse fp 
                    '() 
                    '()
                    #f ;all? 
                    #f ;comment? 
                    0 ;parens 
                    ptbl)))
             (define new-toks 
               (add-tok
                 (list
                    'quote
                    sub)
                    ;(if (and (pair? sub) (dotted? sub))
                    ;     (->dotted-list sub)
                    ;     sub))
                 (get-toks tok toks)))
           ;; Keep going
           (if all?
             (parse fp '() new-toks all? #f parens ptbl)
             (car new-toks))))))
      ((eq? c #\`)
;; TODO: should consolidate this with above
       (cond
         ((and (not all?) (not (null? tok)))
           ;; Reached a terminal char, read out previous token
          (in-port:set-buf! ptbl c)
          (car (add-tok (->tok tok) toks)))
         (else
           ;; Read the next expression and wrap it in a quote
           (let ((sub (parse fp '() '() #f #f 0 ptbl)))
             (define new-toks 
               (add-tok
                 (list 'quasiquote sub)
                 (get-toks tok toks)))
           ;; Keep going
           (if all?
             (parse fp '() new-toks all? #f parens ptbl)
             (car new-toks))))))
      ((eq? c #\,)
       (cond
         ((and (not all?) (not (null? tok)))
           ;; Reached a terminal char, read out previous token
          (in-port:set-buf! ptbl c)
          (car (add-tok (->tok tok) toks)))
         (else
 ;; TODO: 
 ; buffer must be empty now since it is only 1 char, so 
 ; call read-char. then:
 ; - @ - unquote-splicing processing
 ; - eof - error
 ; - otherwise, add char back to buffer and do unquote processing

           ;; Read the next expression and wrap it in a quote
           (letrec ((sub #f)
                    (next-c (read-char fp))
                    (unquote-sym (if (equal? next-c #\@) 'unquote-splicing 'unquote))
                    (new-toks #f))

             ;; Buffer read-ahead char, if unused
             (cond
               ((eof-object? next-c)
                (parse-error "unexpected end of file" 
                  (in-port:get-lnum ptbl)
                  (in-port:get-cnum ptbl)))
               ((not (equal? next-c #\@))
                (in-port:set-buf! ptbl next-c))
               (else #f))

             (set! sub (parse fp '() '() #f #f 0 ptbl))
             (set! new-toks
                  (add-tok
                    (list unquote-sym sub)
                    (get-toks tok toks)))

             ;; Keep going
             (if all?
               (parse fp '() new-toks all? #f parens ptbl)
               (car new-toks))))))
      ((eq? c #\()
       (cond
         ((and (not all?) (not (null? tok)))
          ;; Reached a terminal char, read out previous token
          (in-port:set-buf! ptbl c)
          (car (add-tok (->tok tok) toks)))
         (else
           (let ((sub ;(_cyc-read-all fp (+ parens 1)))
                      (parse fp '() '() #t #f (+ parens 1) ptbl))
                 (toks* (get-toks tok toks)))
             (define new-toks (add-tok 
                                (if (and (pair? sub) (dotted? sub))
                                    (->dotted-list sub)
                                    sub)
                                toks*)) 
;(write `(DEBUG incrementing paren level ,parens ,sub))
             (if all?
              (parse fp '() new-toks all? #f parens ptbl)
              (car new-toks))))))
      ((eq? c #\))
       (cond
         ((and (not all?) (not (null? tok)))
          ;; Reached a terminal char, read out previous token
          (in-port:set-buf! ptbl c)
          (car (add-tok (->tok tok) toks)))
         ((= parens 0)
          (parse-error "unexpected closing parenthesis" 
            (in-port:get-lnum ptbl)
            (in-port:get-cnum ptbl)))
         (else
           (reverse (get-toks tok toks)))))
      ((eq? c #\")
       (cond
         ((and (not all?) (not (null? tok)))
          ;; Reached a terminal char, read out previous token
          (in-port:set-buf! ptbl c)
          (car (add-tok (->tok tok) toks)))
         (else
          (let ((str (read-str fp '() ptbl))
                (toks* (get-toks tok toks)))
            (define new-toks (add-tok str toks*))
            (if all?
             (parse fp '() new-toks all? #f parens ptbl)
             (car new-toks))))))
      ((eq? c #\#)
       (if (null? tok)
         ;; # reader
         (let ((next-c (read-char fp)))
            (in-port:set-cnum! ptbl
                (+ 1 (in-port:get-cnum ptbl)))
            (cond
              ;; Booleans
              ;; Do not use add-tok below, no need to quote a bool
              ((eq? #\t next-c) 
               (if all?
                   (parse fp '() (cons #t toks) all? #f parens ptbl)
                   #t))
              ((eq? #\f next-c) 
               (if all?
                   (parse fp '() (cons #f toks) all? #f parens ptbl)
                   #f))
              ;; Vector
              ((eq? #\( next-c)
               (let ((sub (parse fp '() '() #t #f (+ parens 1) ptbl))
                     (toks* (get-toks tok toks)))
                 (define new-toks 
                   (add-tok 
                     (if (and (pair? sub) (dotted? sub))
                         (parse-error 
                            "Invalid vector syntax" ;(->dotted-list sub)
                            (in-port:get-lnum ptbl)
                            (in-port:get-cnum ptbl))
                         (list->vector sub))
                     toks*)) 
                 (if all?
                  (parse fp '() new-toks all? #f parens ptbl)
                  (car new-toks))))
              ;; Character
              ((eq? #\\ next-c)
               (let ((new-toks (cons (read-pound fp ptbl) toks)))
                 (if all?
                   (parse fp '() new-toks all? #f parens ptbl)
                   (car new-toks))))
              (else
                (parse-error "Unhandled input sequence" 
                  (in-port:get-lnum ptbl)
                  (in-port:get-cnum ptbl)))))
         ;; just another char...
         (parse fp (cons c tok) toks all? #f parens ptbl)))
      (else
        (parse fp (cons c tok) toks all? #f parens ptbl)))))

;; Read chars past a leading #\
(define (read-pound fp ptbl)
  (define (done raw-buf)
    (let ((buf (reverse raw-buf)))
      (cond 
        ((= 0 (length buf))
         (parse-error "missing character" 
           (in-port:get-lnum ptbl)
           (in-port:get-cnum ptbl)))
        ((= 1 (length buf))
         (car buf))
        ((equal? buf '(#\a #\l #\a #\r #\m))
         (integer->char 7))
        ((equal? buf '(#\b #\a #\c #\k #\s #\p #\a #\c #\e))
         (integer->char 8))
        ((equal? buf '(#\d #\e #\l #\e #\t #\e))
         (integer->char 127))
        ((equal? buf '(#\e #\s #\c #\a #\p #\e))
         (integer->char 27))
        ((equal? buf '(#\n #\e #\w #\l #\i #\n #\e))
         (integer->char 10))
        ((equal? buf '(#\n #\u #\l #\l))
         (integer->char 0))
        ((equal? buf '(#\r #\e #\t #\u #\r #\n))
         (integer->char 13))
        ((equal? buf '(#\s #\p #\a #\c #\e))
         (integer->char 32))
        ((equal? buf '(#\t #\a #\b))
         (integer->char 9))
        (else
         (parse-error (string-append 
                        "unable to parse character: "
                        (list->string buf))
                      (in-port:get-lnum ptbl)
                      (in-port:get-cnum ptbl))))))
  (define (loop buf)
    (let ((c (peek-char fp)))
      (if (or (eof-object? c)
              (char-whitespace? c)
              (and (> (length buf) 0)
                   (equal? c #\))))
         (done buf)
         (loop (cons (read-char fp) buf)))))
  (loop '()))

(define (read-str fp buf ptbl)
  (let ((c (read-char fp)))
    (cond
      ((eof-object? c)
       (parse-error "missing closing double-quote" 
         (in-port:get-lnum ptbl)
         (in-port:get-cnum ptbl)))
      ((equal? #\\ c)
        (read-str fp (read-str-esc fp buf ptbl) ptbl))
      ((equal? #\" c)
       (list->string (reverse buf)))
      (else
        (read-str fp (cons c buf) ptbl)))))

;; Read an escaped character within a string
;; The escape '\' has already been read at this point
(define (read-str-esc fp buf ptbl)
  (let ((c (read-char fp)))
    (cond
      ((eof-object? c)
       (parse-error "missing escaped character within string"
         (in-port:get-lnum ptbl)
         (in-port:get-cnum ptbl)))
      ((or (equal? #\" c)
           (equal? #\\ c))
       (cons c buf))
      ((equal? #\n c)
       (cons #\newline buf))
      (else
        (parse-error (string-append 
                       "invalid escape character [" 
                       (list->string (list c))
                       "] in string")
         (in-port:get-lnum ptbl)
         (in-port:get-cnum ptbl))))))

(define (sign? c)
  (or
    (equal? c #\+)
    (equal? c #\-)))

;; parse-atom -> [chars] -> literal
(define (parse-atom a)
  (cond 
    ((or (char-numeric? (car a))
         (and (> (length a) 1)
              (char-numeric? (cadr a))
              (sign? (car a))))
     (string->number (list->string a)))
    (else
     (string->symbol (list->string a)))))

;; Main lexer/parser
(define cyc-read ;; TODO: should be (read), but that is breaking on csi 4.8.0.5
  (lambda args
    (let ((fp (if (null? args)
                  (current-input-port)
                  (car args))))
      (parse fp '() '() #f #f 0 (reg-port fp)))))

;; read-all -> port -> [objects]
(define (read-all . args)
  (let ((fp (if (null? args)
                (current-input-port)
                (car args))))
    (define (loop fp result)
      (let ((obj (cyc-read fp)))
        (if (eof-object? obj)
          (reverse result)
          (loop fp (cons obj result)))))
    (loop fp '())))

;; TODO: for some reason this causes trouble in chicken 4.8. WTF??
;; read -> port -> object
;(define read cyc-read)

;    ;; Test code
;    ;(let ((fp (open-input-file "tests/begin.scm")))
;    ;(let ((fp (open-input-file "tests/strings.scm")))
;     (let ((fp (open-input-file "test.scm")))
;     (let ((fp (open-input-file "tests/unit-tests.scm")))
;      (write (read-all fp)))
;(define (repl)
;    (let ((fp (current-input-port)))
;     (write (cyc-read fp)))
;  (repl))
;(repl)

  ))
