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
          (scheme cyclone common)
          ;(scheme write)
          (scheme char))
  (export
    read
    read-all
    read-all/source
    include
    include-ci)
  (inline
    Cyc-opaque-eq?
    Cyc-opaque-unsafe-eq?
    Cyc-opaque-unsafe-string?
    Cyc-opaque-unsafe-string->number)
  (begin

(define *sym-dot* (string->symbol "."))
(define *sym-unquote-splicing* (string->symbol ",@"))
(define *sym-datum-comment* (string->symbol "#;"))

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
                (read-all/source port filename))))
          (cdr expr)))))))

(define-syntax include-ci
  (er-macro-transformer
   (lambda (expr rename compare)
    `(include ,@(cdr expr)))))

;; Convert a list read by the reader into an improper list
(define (->dotted-list lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) *sym-dot*)
     (cadr lst))
    (else
      (cons (car lst) (->dotted-list (cdr lst))))))
  
;; Main lexer/parser
(define read
  (lambda args
    (let ((fp (if (null? args)
                  (current-input-port)
                    (car args)))
          (ssi! (if (and (pair? args)
                         (pair? (cdr args)))
                    (cadr args)
                    #f)) ;; Default
          (fname (if (and (pair? args)
                          (pair? (cdr args))
                          (pair? (cddr args)))
                     (caddr args)
                     #f)))
      (let ((result (parse fp ssi! fname)))
        (if (Cyc-opaque? result)
          (read-error fp fname "unexpected closing parenthesis")
          result)))))

;; Read given file, collecting source location information so we
;; can give meaningful compiler error messages
(define (read-all/source port filename)
  (read-all port store-source-info! filename))

;; read-all -> port -> [objects]
(define (read-all . args)
  (let* ((fp (if (null? args)
                 (current-input-port)
                 (car args)))
         (ssi! (if (and (pair? args)
                        (pair? (cdr args)))
                   (cadr args)
                   #f)) ;; Default
         (fname (if (and ssi!
                         (pair? (cddr args)))
                    (caddr args)
                    #f)))
    (define (loop fp result)
      (let ((obj (read fp ssi! fname)))
        (if (eof-object? obj)
          (reverse result)
          (loop fp (cons obj result)))))
    (loop fp '())))

;;(define-c reading-from-file?
;;  "(void *data, int argc, closure _, object k, object port)"
;;  " object result = boolean_f;
;;    Cyc_check_port(data, port);
;;    if (((port_type *)port)->flags == 1) {
;;      result = boolean_t;
;;    }
;;    return_closcall1(data, k, result);")

(define-c read-token
  "(void *data, int argc, closure _, object k, object port)"
  " Cyc_io_read_token(data, k, port);")

(define-c read-error
  "(void *data, int argc, closure _, object k, object port, object filename, object msg)"
  " char buf[1024];
    port_type *p;
    Cyc_check_port(data, port);
    Cyc_check_str(data, msg);
    p = ((port_type *)port);
    if (Cyc_is_string(filename) == boolean_t) {
      snprintf(buf, 1023, \"at line %d, column %d of %s: %s\", 
             p->line_num, p->col_num, 
             string_str(filename), 
             string_str(msg));
    } else {
      snprintf(buf, 1023, \"at line %d, column %d: %s\", 
             p->line_num, p->col_num, string_str(msg));
    }
    Cyc_rt_raise_msg(data, buf);")

(define-c Cyc-opaque-eq?
  "(void *data, int argc, closure _, object k, object opq, object obj)"
  " if (Cyc_is_opaque(opq) == boolean_f) 
      return_closcall1(data, k, boolean_f);
    return_closcall1(data, k, equalp( opaque_ptr(opq), obj ));"
  "(void *data, object ptr, object opq, object obj)"
  " if (Cyc_is_opaque(opq) == boolean_f) 
      return(boolean_f);
    return(equalp( opaque_ptr(opq), obj ));")

(define-c Cyc-opaque-unsafe-eq?
  "(void *data, int argc, closure _, object k, object opq, object obj)"
  " return_closcall1(data, k, equalp( opaque_ptr(opq), obj ));"
  "(void *data, object ptr, object opq, object obj)"
  " return(equalp( opaque_ptr(opq), obj ));")

(define-c Cyc-opaque-unsafe-string?
  "(void *data, int argc, closure _, object k, object opq)"
  " return_closcall1(data, k, Cyc_is_string(opaque_ptr(opq)));"
  "(void *data, object ptr, object opq)"
  " return(Cyc_is_string(opaque_ptr(opq)));")

(define-c Cyc-opaque-unsafe-string->number
  "(void *data, int argc, closure _, object k, object opq)"
  " Cyc_string2number_(data, k, opaque_ptr(opq));")

(define-c Cyc-make-rect
  "(void *data, int argc, closure _, object k, object r, object i)"
  " Cyc_make_rectangular(data, k, r, i); ")

(define-c get-line-num 
  "(void *data, int argc, closure _, object k, object port)"
  " int num = ((port_type *)port)->line_num; 
   return_closcall1(data, k, obj_int2obj(num)); ")

(define-c get-col-num 
  "(void *data, int argc, closure _, object k, object port)"
  " int num = ((port_type *)port)->col_num; 
   return_closcall1(data, k, obj_int2obj(num)); ")

(define (store-source-info! obj filename line col) 
  (set! *reader-source-db* 
        (cons (cons obj (vector filename line col))
              *reader-source-db*)))
  ;; TODO: where to store? Need to use a hashtable but also needs to
  ;; be accessible from macro's. probably needs to be in global env,
  ;; see (cyclone foreign) for an example
;; TODO: need corresponding macro (syntax-error/source ??) to use this
;; information for error reporting
;; TODO: probably want to have a top-level exception handler on macro
;; expansion and call compiler error to report any error along with 
;; source info


;; Parse given input port and retrieve next token
;;
;; Params:
;;  fp - Input port
;;  ssi! - "Store Source Info" function, or #f if none
;;  fname - Filename being read, or #f if N/A
;;
;; Returns read token
(define (parse fp ssi! fname)
  (let ((token (read-token fp)))
    ;;(display "//")(write `(token ,token)) (newline)
    (cond
      ((Cyc-opaque? token)
       (cond
         ((Cyc-opaque-unsafe-string? token)
          (Cyc-opaque-unsafe-string->number token))
         ;; Open paren, start read loop
         ((Cyc-opaque-unsafe-eq? token #\()
          (let ((line-num (get-line-num fp))
                (col-num (get-col-num fp)))
            (let loop ((lis '())
                       (t (parse fp ssi! fname)))
              (cond
                ((eof-object? t)
                 (read-error fp fname "missing closing parenthesis"))
                ((Cyc-opaque-eq? t #\))
                 (if (and (> (length lis) 2)
                          (equal? (cadr lis) *sym-dot*))
                     (->dotted-list (reverse lis))
                     (let ((result (reverse lis)))
                       (when (and ssi!
                                  (pair? result)
                                  (symbol? (car result)))
                         ;; Possible macro expansion, save source info
                         (ssi! result 
                               fname 
                               line-num 
                               (- col-num 1))) ;; Account for open paren
                       result)))
                (else
                 (loop (cons t lis) (parse fp ssi! fname)))))))
         ((Cyc-opaque-unsafe-eq? token #\')
          (list 'quote (parse fp ssi! fname)))
         ((Cyc-opaque-unsafe-eq? token #\`)
          (list 'quasiquote (parse fp ssi! fname)))
         ((Cyc-opaque-unsafe-eq? token #\,)
          (list 'unquote (parse fp ssi! fname)))
         (else
          token))) ;; error if this is returned to original caller of parse
      ((vector? token)
       (cond
        ((= (vector-length token) 3) ;; Special case: exact/inexact number
         (if (vector-ref token 2)
             (exact (string->number (vector-ref token 0) (vector-ref token 1)))
             (inexact (string->number (vector-ref token 0) (vector-ref token 1)))))
        ((= (vector-length token) 2) ;; Special case: special symbols
         (let ((t (vector-ref token 0)))
           (cond
             ((eq? t *sym-unquote-splicing*)
              (list 'unquote-splicing (parse fp ssi! fname)))
             ((eq? t *sym-datum-comment*)
              (parse fp ssi! fname) ;; Ignore next datum
              (parse fp ssi! fname))
             ((string? t) ;; Special case: complex number
              (let* ((end (vector-ref token 1))
                     (len (string-length t))
                     (only-imag? (= (+ 1 end) len)) ;; EG: "57i" with no real component
                     (real-str (if only-imag?
                                   "0"
                                   (substring t 0 end)))
                     (imag-str (if only-imag?
                                   (substring t 0 end)
                                   (substring t end (- len 1))))
                     (real (string->number real-str))
                     (imag (string->number imag-str))
                    )
                (Cyc-make-rect real imag)))
             (else
              (error "Unexpected token" t)))))
        ((= (vector-length token) 1) ;; Special case: error
         (error (vector-ref token 0)))
        (else
         (let loop ((lis '())
                    (t (parse fp ssi! fname)))
           (cond
             ((eof-object? t)
              (read-error fp fname "missing closing parenthesis"))
             ((Cyc-opaque-eq? t #\))
              (list->vector (reverse lis)))
             (else
              (loop (cons t lis) (parse fp ssi! fname))))))))
      ((bytevector? token)
       (let loop ((lis '())
                  (t (parse fp ssi! fname)))
         (cond
           ((eof-object? t)
            (read-error fp fname "missing closing parenthesis"))
           ((Cyc-opaque-eq? t #\))
            (apply bytevector (reverse lis)))
           (else
            (loop (cons t lis) (parse fp ssi! fname))))))
      (else
        token))))
  ))
