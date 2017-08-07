;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the s-expression parser and supporting functions.
;;;;
(define-library (scheme read)
  (import (scheme base)
          ;(scheme write)
          (scheme char))
  (export
    read
    read-all
    include
    include-ci
  )
  (inline
    in-port:get-cnum
    in-port:get-lnum
    in-port:get-buf
  )
  (begin

(define-syntax include
  (er-macro-transformer
   (lambda (expr rename compare)
     (apply
      append
      (cons
        '(begin)
         (map
          (lambda (filename)
            (call-with-port
              (open-input-file filename)
              (lambda (port)
                (read-all port))))
          (cdr expr)))))))

(define-syntax include-ci
  (er-macro-transformer
   (lambda (expr rename compare)
    `(include ,@(cdr expr)))))

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

(define (in-port:get-buf ptbl) (cadr ptbl))
(define (in-port:get-lnum ptbl) (caddr ptbl))
(define (in-port:get-cnum ptbl) (cadddr ptbl))
;(define in-port:get-buf cadr)
;(define in-port:get-lnum caddr)
;(define in-port:get-cnum cadddr)
(define (in-port:set-buf! ptbl buf) (set-car! (cdr ptbl) buf))
(define (in-port:set-lnum! ptbl lnum) (set-car! (cddr ptbl) lnum))
(define (in-port:set-cnum! ptbl cnum) (set-car! (cdddr ptbl) cnum))
;(define-syntax in-port:set-buf! 
;  (er-macro-transformer
;    (lambda (e r c)
;      `(set-car! (cdr ,(cadr e)) ,(caddr e)))))
;(define-syntax in-port:set-lnum!
;  (er-macro-transformer
;    (lambda (e r c)
;      `(set-car! (cddr ,(cadr e)) ,(caddr e)))))
;(define-syntax in-port:set-cnum!
;  (er-macro-transformer
;    (lambda (e r c)
;      `(set-car! (cdddr ,(cadr e)) ,(caddr e)))))
(define (in-port:read-buf! ptbl)
 (let ((result (cadr ptbl)))
   (in-port:set-buf! ptbl #f)
   result))
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
              (in-port:set-lnum! ptbl (+ 1 (in-port:get-lnum ptbl)))
              (in-port:set-cnum! ptbl 0)
              (parse fp '() toks all? #f parens ptbl))
           (parse fp '() toks all? #t parens ptbl)))
      ((eq? c #\newline)
       (in-port:set-lnum! ptbl (+ 1 (in-port:get-lnum ptbl)))
       (in-port:set-cnum! ptbl 0)
       (parse/tok fp tok toks all? #f parens ptbl c))
      ((char-whitespace? c)
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
              ;; Block comments
              ((eq? #\| next-c)
               (read-block-comment fp ptbl)
               (parse fp '() toks all? #f parens ptbl))
              ;; Booleans
              ;; Do not use add-tok below, no need to quote a bool
              ((eq? #\t next-c) 
               ;; read in rest of #true if it is there
               (when (eq? #\r (peek-char fp))
                 (if (not (and (eq? #\r (read-char fp))
                               (eq? #\u (read-char fp))
                               (eq? #\e (read-char fp))))
                     (parse-error "Invalid syntax for boolean true" 
                       (in-port:get-lnum ptbl)
                       (in-port:get-cnum ptbl))))
               (if all?
                   (parse fp '() (cons #t toks) all? #f parens ptbl)
                   #t))
              ((eq? #\f next-c) 
               ;; read in rest of #false if it is there
               (when (eq? #\a (peek-char fp))
                 (if (not (and (eq? #\a (read-char fp))
                               (eq? #\l (read-char fp))
                               (eq? #\s (read-char fp))
                               (eq? #\e (read-char fp))))
                     (parse-error "Invalid syntax for boolean false" 
                       (in-port:get-lnum ptbl)
                       (in-port:get-cnum ptbl))))
               (if all?
                   (parse fp '() (cons #f toks) all? #f parens ptbl)
                   #f))
              ;; Numbers
              ((eq? #\e next-c)
               (parse-number fp toks all? parens ptbl 
                 10 (lambda (num)
                      (exact
                        (string->number (list->string num))))))
              ((eq? #\i next-c)
               (parse-number fp toks all? parens ptbl 
                 10 (lambda (num)
                      (inexact
                        (string->number (list->string num))))))
              ((eq? #\b next-c)
               (parse-number fp toks all? parens ptbl 
                 2 (lambda (num) (string->number (list->string num) 2))))
              ((eq? #\o next-c)
               (parse-number fp toks all? parens ptbl 
                 8 (lambda (num) (string->number (list->string num) 8))))
              ((eq? #\x next-c)
               (parse-number fp toks all? parens ptbl 
                 16 (lambda (num) (string->number (list->string num) 16))))
              ;; Bytevector
              ((eq? #\u next-c)
                (set! next-c (read-char fp))
                (if (not (eq? #\8 next-c))
                    (parse-error "Unhandled input sequence" 
                      (in-port:get-lnum ptbl)
                      (in-port:get-cnum ptbl)))
                (set! next-c (read-char fp))
                (if (not (eq? #\( next-c))
                    (parse-error "Unhandled input sequence" 
                      (in-port:get-lnum ptbl)
                      (in-port:get-cnum ptbl)))
               (let ((sub (parse fp '() '() #t #f (+ parens 1) ptbl))
                     (toks* (get-toks tok toks)))
                 (define new-toks 
                   (add-tok 
                     (if (and (pair? sub) (dotted? sub))
                         (parse-error 
                            "Invalid vector syntax" ;(->dotted-list sub)
                            (in-port:get-lnum ptbl)
                            (in-port:get-cnum ptbl))
                         (apply bytevector sub))
                     toks*)) 
                 (if all?
                  (parse fp '() new-toks all? #f parens ptbl)
                  (car new-toks))))
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
              ;; Datum comment
              ((eq? #\; next-c)
               ; Read and discard next datum
               (parse fp '() '() #f #f 0 ptbl)
               (cond
                 ((and (not all?) (not (null? tok)))
                  ;; Reached a terminal char, read out previous token
                  (in-port:set-buf! ptbl c)
                  (car (add-tok (->tok tok) toks)))
                 (else
                  (parse fp tok toks all? #f parens ptbl))))
              (else
                (parse-error "Unhandled input sequence" 
                  (in-port:get-lnum ptbl)
                  (in-port:get-cnum ptbl)))))
         ;; just another char...
         (parse fp (cons c tok) toks all? #f parens ptbl)))
      ((eq? c #\|)
       (parse-literal-identifier fp toks all? parens ptbl))
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
        ((and (> (length buf) 1)
              (equal? (car buf) #\x))
         (integer->char (string->number (list->string (cdr buf)) 16)))
        (else
         (parse-error (string-append 
                        "unable to parse character: "
                        (list->string buf))
                      (in-port:get-lnum ptbl)
                      (in-port:get-cnum ptbl))))))
  (define (loop buf)
    (let ((c (peek-char fp)))
      (cond
        ((or (eof-object? c)
             (and (char-whitespace? c) (> (length buf) 0))
             (and (> (length buf) 0)
                  (equal? c #\))))
         (done buf))
       (else
         (loop (cons (read-char fp) buf))))))
  (loop '()))

(define (read-hex-scalar-value fp ptbl)
  (define (done buf)
    (cond 
      ((= 0 (length buf))
       (parse-error "missing character" 
         (in-port:get-lnum ptbl)
         (in-port:get-cnum ptbl)))
      (else
       (integer->char (string->number (list->string buf) 16)))))
  (define (loop buf)
    (let ((c (read-char fp)))
      (cond
        ((or (eof-object? c)
             (eq? #\; c))
         (done (reverse buf)))
       (else
         (loop (cons c buf))))))
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
           (equal? #\' c)
           (equal? #\? c)
           (equal? #\| c)
           (equal? #\\ c))
       (cons c buf))
      ((equal? #\a c) (cons #\alarm buf))
      ((equal? #\b c) (cons #\backspace buf))
      ((equal? #\n c) (cons #\newline buf))
      ((equal? #\r c) (cons #\return buf))
      ((equal? #\t c) (cons #\tab buf))
      ((equal? #\x c)
       (cons (read-hex-scalar-value fp ptbl) buf))
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

;; token-numeric? -> [chars] -> boolean
(define (token-numeric? a)
    (or (char-numeric? (car a))
        (and (> (length a) 1)
             (eq? #\. (car a))
             (char-numeric? (cadr a)))
        (and (> (length a) 1)
             (or (char-numeric? (cadr a))
                 (eq? #\. (cadr a)))
             (sign? (car a)))))

;; parse-atom -> [chars] -> literal
(define (parse-atom a)
  (if (token-numeric? a)
      (string->number (list->string a))
      (if (or (equal? a '(#\+ #\i #\n #\f #\. #\0))
              (equal? a '(#\- #\i #\n #\f #\. #\0)))
          (expt 2.0 1000000)
          (if (or (equal? a '(#\+ #\n #\a #\n #\. #\0))
                  (equal? a '(#\- #\n #\a #\n #\. #\0)))
              (/ 0.0 0.0)
              (string->symbol (list->string a))))))

;;;;;
;; Read next character from port, using buffered char if available
(define (get-next-char fp ptbl)
  (if (in-port:get-buf ptbl)
      (in-port:read-buf! ptbl) ;; Already buffered
      (read-char fp)))

;; Read chars in the middle of a block comment
(define (read-block-comment fp ptbl)
  (let ((c (get-next-char fp ptbl)))
    (cond
      ((eq? #\| c) (read-block-terminator fp ptbl))
      (else (read-block-comment fp ptbl)))))

;; Read (possibly) the end of a block comment
(define (read-block-terminator fp ptbl)
  (let ((c (get-next-char fp ptbl)))
    (cond
      ((eq? #\# c) #t)
      ((eq? #\| c) (read-block-terminator fp ptbl))
      (else (read-block-comment fp ptbl)))))

;; Parse literal identifier encountered within pipes
(define (parse-literal-identifier fp toks all? parens ptbl)
  (let ((sym (parse-li-rec fp '() ptbl)))
    (if all?
      (parse fp '() (cons sym toks) all? #f parens ptbl)
      sym)))

;; Helper for parse-literal-identifier
(define (parse-li-rec fp tok ptbl)
  (let ((c (get-next-char fp ptbl))
        (next (lambda (c) (parse-li-rec fp (cons c tok) ptbl))))
    (cond
      ((eq? #\| c) 
       (let ((str (if (null? tok) 
                      ""
                      (list->string 
                        (reverse tok)))))
        (string->symbol str)))
      ((eof-object? c) 
       (parse-error "EOF encountered parsing literal identifier" 
         (in-port:get-lnum ptbl)
         (in-port:get-cnum ptbl)))
      (else
        (next c)))))

(define (parse-number fp toks all? parens ptbl base tok->num)
;  (parse-number-rec base fp '() ptbl))
  (let ((num (parse-number-rec base fp '() ptbl)))
    ;(write `(DEBUG2 ,num ,(string? num)))
    (cond
      ((and (not (null? num))
            (or (token-numeric? num)
                (and (> (length num) 0)
                     (= base 16)
                     (hex-digit? (car num)))))
       (let ((result (tok->num num)))
         (if all?
             (parse fp '() (cons result toks) all? #f parens ptbl)
             result)))
      (else
       (parse-error
         "Illegal number syntax"
         (in-port:get-lnum ptbl)
         (in-port:get-cnum ptbl))))))

(define (parse-number-rec base fp tok ptbl)
  (let ((c (get-next-char fp ptbl))
        (next (lambda (c) (parse-number-rec base fp (cons c tok) ptbl))))
    (cond
      ((sign? c) (next c))
      ((eq? #\. c) (next c))
      ((char-numeric? c) 
       (if (or (and (= base 2) (char>? c #\1))
               (and (= base 8) (char>? c #\7)))
           (parse-error
             "Illegal digit"
             (in-port:get-lnum ptbl)
             (in-port:get-cnum ptbl)))
       (next c))
      ((and (= base 16) (hex-digit? c))
       (next c))
      (else
        ;; We are done parsing a number
        (in-port:set-buf! ptbl c) ;; rebuffer unprocessed char
        (reverse tok))))) ;; Return token

(define (hex-digit? c)
  (or (and (char>=? c #\a) (char<=? c #\f))
      (and (char>=? c #\A) (char<=? c #\F))))
;;;;;
  
;; Main lexer/parser
(define cyc-read
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
