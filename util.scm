;;
;; Cyclone Scheme
;; Copyright (c) 2015, Justin Ethier
;; All rights reserved.
;;
;; This module contains various utility functions.
;;

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

