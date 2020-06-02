;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains a pretty-printer based on code from:
;;;; http://www.mathematik.uni-muenchen.de/~logik/download/minlog/src/pp-sexp.scm
;;;;
(define-library (scheme cyclone pretty-print)
    (export pretty-print)
    (import (scheme base)
            (scheme write))
    (begin
      (define pretty-print #f)
      (define pp-width 80) ;; TODO: make configurable?
      
      ;;-------------------------------------------------------------------
      ;; We define a pretty printer for Scheme S-expressions (sexp). While
      ;; Petite Scheme supports that by its own, mzscheme does not. If you
      ;; get a sexp (like from proof-to-expr) prefix it with a call to spp and
      ;; the output is nicely formated to fit into pp-width many columns:
      ;;
      ;;  (spp (proof-to-expr (current-proof)))
      ;;
      ;; If you want the sexp printed as a comment, use the
      ;; sexp-pretty-print->string function:
      ;;
      ;;  (display-comment (sexp-pretty-print->string
      ;;                     (proof-to-expr (current-proof))))
      ;;
      
      ;;"genwrite.scm" generic write used by pretty-print and truncated-print.
      ;; Copyright (c) 1991, Marc Feeley
      ;; Author: Marc Feeley (feeley@iro.umontreal.ca)
      ;; Distribution restrictions: none
      ;;
      ;; Modified for Minlog by Stefan Schimanski <schimans@math.lmu.de>
      ;; Taken from slib 2d6, genwrite.scm and pp.scm
      
      (define genwrite:newline-str (make-string 1 #\newline))
      
      (define (generic-write obj display? width output)
      
        (define (read-macro? l)
          (define (length1? l) (and (pair? l) (null? (cdr l))))
          (let ((head (car l)) (tail (cdr l)))
            (case head
              ((quote quasiquote unquote unquote-splicing) (length1? tail))
              (else                                        #f))))
      
        (define (read-macro-body l)
          (cadr l))
      
        (define (read-macro-prefix l)
          (let ((head (car l)) (tail (cdr l)))
            (case head
              ((quote)            "'")
              ((quasiquote)       "`")
              ((unquote)          ",")
              ((unquote-splicing) ",@"))))
      
        (define (out str col)
          (and col (output str) (+ col (string-length str))))
      
        (define (wr obj col)
      
          (define (wr-expr expr col)
            (if (read-macro? expr)
              (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
              (wr-lst expr col)))
      
          (define (wr-lst l col)
            (if (pair? l)
      	  (let loop ((l (cdr l))
      		     (col (and col (wr (car l) (out "(" col)))))
      	    (cond ((not col) col)
      		  ((pair? l)
      		   (loop (cdr l) (wr (car l) (out " " col))))
      		  ((null? l) (out ")" col))
      		  (else      (out ")" (wr l (out " . " col))))))
      	  (out "()" col)))
      
          (cond ((pair? obj)        (wr-expr obj col))
                ((null? obj)        (wr-lst obj col))
                ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
                ((bytevector? obj)  (wr-lst 
                                      (map char->integer (string->list (utf8->string obj)))
                                      (out "#u8" col)))
                ((boolean? obj)     (out (if obj "#t" "#f") col))
                ((number? obj)      (out (number->string obj) col))
                ((symbol? obj)      (out (symbol->string obj) col))
                ((procedure? obj)   (out "#[procedure]" col))
                ((string? obj)      (if display?
                                      (out obj col)
                                      (let loop ((i 0) (j 0) (col (out "\"" col)))
                                        (if (and col (< j (string-length obj)))
                                          (let ((c (string-ref obj j)))
                                            (if (or (char=? c #\\)
                                                    (char=? c #\"))
                                              (loop j
                                                    (+ j 1)
                                                    (out "\\"
                                                         (out (substring obj i j)
                                                              col)))
                                              (loop i (+ j 1) col)))
                                          (out "\""
                                               (out (substring obj i j) col))))))
                ((char? obj)        (if display?
                                      (out (make-string 1 obj) col)
                                      (out (case obj
                                             ((#\space)   "space")
                                             ((#\newline) "newline")
                                             (else        (make-string 1 obj)))
                                           (out "#\\" col))))
                ((input-port? obj)  (out "#[input-port]" col))
                ((output-port? obj) (out "#[output-port]" col))
                ((eof-object? obj)  (out "#[eof-object]" col))
                (else               (out "#[unknown]" col))))
      
        (define (pp obj col)
      
          (define (spaces n col)
            (if (> n 0)
              (if (> n 7)
                (spaces (- n 8) (out "        " col))
                (out (substring "        " 0 n) col))
              col))
      
          (define (indent to col)
            (and col
                 (if (< to col)
                   (and (out genwrite:newline-str col) (spaces to 0))
                   (spaces (- to col) col))))
      
          (define (pr obj col extra pp-pair)
            (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
              (let ((result '())
                    (left (min (+ (- (- width col) extra) 1) max-expr-width)))
                (generic-write obj display? #f
                  (lambda (str)
                    (set! result (cons str result))
                    (set! left (- left (string-length str)))
                    (> left 0)))
                (if (> left 0) ; all can be printed on one line
                  (out (reverse-string-append result) col)
                  (if (pair? obj)
                    (pp-pair obj col extra)
                    (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
              (wr obj col)))
      
          (define (pp-expr expr col extra)
            (if (read-macro? expr)
              (pr (read-macro-body expr)
                  (out (read-macro-prefix expr) col)
                  extra
                  pp-expr)
              (let ((head (car expr)))
                (if (symbol? head)
                  (let ((proc (style head)))
                    (if proc
                      (proc expr col extra)
                      (if (> (string-length (symbol->string head))
                             max-call-head-width)
                        (pp-general expr col extra #f #f #f pp-expr)
                        (pp-call expr col extra pp-expr))))
                  (pp-list expr col extra pp-expr)))))
      
          ; (head item1
          ;       item2
          ;       item3)
          (define (pp-call expr col extra pp-item)
            (let ((col* (wr (car expr) (out "(" col))))
              (and col
                   (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))
      
          ; (item1
          ;  item2
          ;  item3)
          (define (pp-list l col extra pp-item)
            (let ((col (out "(" col)))
              (pp-down l col col extra pp-item)))
      
          (define (pp-down l col1 col2 extra pp-item)
            (let loop ((l l) (col col1))
              (and col
                   (cond ((pair? l)
                          (let ((rest (cdr l)))
                            (let ((extra (if (null? rest) (+ extra 1) 0)))
                              (loop rest
                                    (pr (car l) (indent col2 col) extra pp-item)))))
                         ((null? l)
                          (out ")" col))
                         (else
                          (out ")"
                               (pr l
                                   (indent col2 (out "." (indent col2 col)))
                                   (+ extra 1)
                                   pp-item)))))))
      
          (define (pp-general expr col extra named? pp-1 pp-2 pp-3)
      
            (define (tail1 rest col1 col2 col3)
              (if (and pp-1 (pair? rest))
                (let* ((val1 (car rest))
                       (rest (cdr rest))
                       (extra (if (null? rest) (+ extra 1) 0)))
                  (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
                (tail2 rest col1 col2 col3)))
      
            (define (tail2 rest col1 col2 col3)
              (if (and pp-2 (pair? rest))
                (let* ((val1 (car rest))
                       (rest (cdr rest))
                       (extra (if (null? rest) (+ extra 1) 0)))
                  (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
                (tail3 rest col1 col2)))
      
            (define (tail3 rest col1 col2)
              (pp-down rest col2 col1 extra pp-3))
      
            (let* ((head (car expr))
                   (rest (cdr expr))
                   (col* (wr head (out "(" col))))
              (if (and named? (pair? rest))
                (let* ((name (car rest))
                       (rest (cdr rest))
                       (col** (wr name (out " " col*))))
                  (tail1 rest (+ col indent-general) col** (+ col** 1)))
                (tail1 rest (+ col indent-general) col* (+ col* 1)))))
      
          (define (pp-expr-list l col extra)
            (pp-list l col extra pp-expr))
      
          (define (pp-LAMBDA expr col extra)
            (pp-general expr col extra #f pp-expr-list #f pp-expr))
      
          (define (pp-IF expr col extra)
            (pp-general expr col extra #f pp-expr #f pp-expr))
      
          (define (pp-COND expr col extra)
            (pp-call expr col extra pp-expr-list))
      
          (define (pp-CASE expr col extra)
            (pp-general expr col extra #f pp-expr #f pp-expr-list))
      
          (define (pp-AND expr col extra)
            (pp-call expr col extra pp-expr))
      
          (define (pp-LET expr col extra)
            (let* ((rest (cdr expr))
                   (named? (and (pair? rest) (symbol? (car rest)))))
              (pp-general expr col extra named? pp-expr-list #f pp-expr)))
      
          (define (pp-BEGIN expr col extra)
            (pp-general expr col extra #f #f #f pp-expr))
      
          (define (pp-DO expr col extra)
            (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))
      
          ; define formatting style (change these to suit your style)
      
          (define indent-general 2)
      
          (define max-call-head-width 5)
      
          (define max-expr-width 50)
      
          (define (style head)
            (case head
              ((lambda let* letrec define) pp-LAMBDA)
              ((if set!)                   pp-IF)
              ((cond)                      pp-COND)
              ((case)                      pp-CASE)
              ((and or)                    pp-AND)
              ((let)                       pp-LET)
              ((begin)                     pp-BEGIN)
              ((do)                        pp-DO)
              (else                        #f)))
      
          (pr obj col 0 pp-expr))
      
        (if width
          (out genwrite:newline-str (pp obj 0))
          (wr obj 0)))
      
      ; (reverse-string-append l) = (apply string-append (reverse l))
      
      (define (reverse-string-append l)
        (apply string-append (reverse l)))
        ;; JAE - Avoid issue with string-set and multibyte chars.
        ;;       This should be more efficient as well
        ;(define (rev-string-append l i)
        ;  (if (pair? l)
        ;    (let* ((str (car l))
        ;           (len (string-length str))
        ;           (result (rev-string-append (cdr l) (+ i len))))
        ;      (let loop ((j 0) (k (- (- (string-length result) i) len)))
        ;        (if (< j len)
        ;          (begin
        ;            (string-set! result k (string-ref str j))
        ;            (loop (+ j 1) (+ k 1)))
        ;          result)))
        ;    (make-string i)))
      
        ;(rev-string-append l 0))
      
      (define (sexp-pretty-print obj . opt)
        (let ((port (if (pair? opt) (car opt) (current-output-port))))
          (generic-write obj #f pp-width
      		   (lambda (s) (display s port) #t))
          (display "")))
      
      (define (sexp-pretty-print->string obj . width)
        (define result '())
        (generic-write obj #f (if (null? width) pp-width (car width))
      		 (lambda (str) (set! result (cons str result)) #t))
        (reverse-string-append result))
      
      (define spp sexp-pretty-print)
      (set! pretty-print spp)
      ;;-------------------------------------------------------------------
))

