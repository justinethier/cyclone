;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the char library from r7rs.
;;;;
(define-library (scheme char)
  (export
    char-alphabetic?
    char-downcase
    char-foldcase
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
    char-ci<=?
    char-ci<? 
    char-ci=?
    char-ci>=? 
    char-ci>?
    digit-value
    string-upcase
    string-downcase
    string-foldcase
    string-ci<=? 
    string-ci<?
    string-ci=? 
    string-ci>=?
    string-ci>? 
  )
  (import (scheme base))
  (begin
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
    (define char-foldcase char-downcase) ;; Good enough for now, since no Unicode yet
    (define (char-ci=? c1 c2 . cs)  (apply char=? (map char-foldcase (cons c1 (cons c2 cs)))))
    (define (char-ci<=? c1 c2 . cs) (apply char<=? (map char-foldcase (cons c1 (cons c2 cs)))))
    (define (char-ci<? c1 c2 . cs)  (apply char<? (map char-foldcase (cons c1 (cons c2 cs)))))
    (define (char-ci>=? c1 c2 . cs) (apply char>=? (map char-foldcase (cons c1 (cons c2 cs)))))
    (define (char-ci>? c1 c2 . cs)  (apply char>? (map char-foldcase (cons c1 (cons c2 cs)))))
    (define (string-ci=? s1 s2) (apply string=? (map string-foldcase (list s1 s2))))
    (define (string-ci<? s1 s2) (apply string<? (map string-foldcase (list s1 s2))))
    (define (string-ci<=? s1 s2) (apply string<=? (map string-foldcase (list s1 s2))))
    (define (string-ci>? s1 s2) (apply string>? (map string-foldcase (list s1 s2))))
    (define (string-ci>=? s1 s2) (apply string>=? (map string-foldcase (list s1 s2))))
    (define (char-alphabetic? c) (or (char-upper-case? c) (char-lower-case? c)))
    (define (char-upper-case? c) (and (char>=? c #\A) (char<=? c #\Z))) ;; ASCII-only
    (define (char-lower-case? c) (and (char>=? c #\a) (char<=? c #\z))) ;; ASCII-only
    (define (char-numeric? c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (define (char-whitespace? c) (member c '(#\tab #\space #\return #\newline)))
    (define (digit-value c)
      (if (char-numeric? c)
          (- (char->integer c) (char->integer #\0))
          #f))
    (define (string-upcase str) (string-map char-upcase str))
    (define (string-downcase str) (string-map char-downcase str))
    (define (string-foldcase str) (string-map char-foldcase str))
))
