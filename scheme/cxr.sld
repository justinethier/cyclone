;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; The cxr library from r7rs.
;;;;
(define-library (scheme cxr)
  (export
    caar
    cadr
    cdar
    cddr
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr
    caaaaar)
  (begin
     (define (caar lis)   (car(car lis)))
     (define (cadr lis)   (car(cdr lis)))
     (define (cdar lis)   (cdr(car lis)))
     (define (cddr lis)   (cdr(cdr lis)))
     (define (caaar lis)  (car(car(car lis))))
     (define (caadr lis)  (car(car(cdr lis))))
     (define (cadar lis)  (car(cdr(car lis))))
     (define (caddr lis)  (car(cdr(cdr lis))))
     (define (cdaar lis)  (cdr(car(car lis))))
     (define (cdadr lis)  (cdr(car(cdr lis))))
     (define (cddar lis)  (cdr(cdr(car lis))))
     (define (cdddr lis)  (cdr(cdr(cdr lis))))
     (define (caaaar lis) (car(car(car(car lis)))))
     (define (caaadr lis) (car(car(car(cdr lis)))))
     (define (caadar lis) (car(car(cdr(car lis)))))
     (define (caaddr lis) (car(car(cdr(cdr lis)))))
     (define (cadaar lis) (car(cdr(car(car lis)))))
     (define (cadadr lis) (car(cdr(car(cdr lis)))))
     (define (caddar lis) (car(cdr(cdr(car lis)))))
     (define (cadddr lis) (car(cdr(cdr(cdr lis)))))
     (define (cdaaar lis) (cdr(car(car(car lis)))))
     (define (cdaadr lis) (cdr(car(car(cdr lis)))))
     (define (cdadar lis) (cdr(car(cdr(car lis)))))
     (define (cdaddr lis) (cdr(car(cdr(cdr lis)))))
     (define (cddaar lis) (cdr(cdr(car(car lis)))))
     (define (cddadr lis) (cdr(cdr(car(cdr lis)))))
     (define (cdddar lis) (cdr(cdr(cdr(car lis)))))
     (define (cddddr lis) (cdr(cdr(cdr(cdr lis)))))
    ;; Non-standard, this is just a placeholder
    (define (caaaaar lis)
      (car (car (car (car (car lis))))))))
