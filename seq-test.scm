;; This is a temporary file for testing
(import (scheme base) (scheme write)
(scheme cyclone transforms)
)

(define-c symbol<?
  "(void *data, int argc, closure _, object k, object sym1, object sym2)"
  "
     Cyc_check_sym(data, sym1);
     Cyc_check_sym(data, sym2);
     object result = (strcmp(symbol_desc(sym1), symbol_desc(sym2)) < 0) 
                     ? boolean_t : boolean_f;
     return_closcall1(data, k, result);
  "
  "(void *data, object ptr, object sym1, object sym2)"
  " 
     Cyc_check_sym(data, sym1);
     Cyc_check_sym(data, sym2);
     object result = (strcmp(symbol_desc(sym1), symbol_desc(sym2)) < 0) 
                     ? boolean_t : boolean_f;
     return result;
  ")

(define (insert sym S)
  (if (not (pair? S))
      (list sym)
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        (else (insert sym (cdr S))))))
        ;(else (cons (car S) (insert sym (cdr S)))))))
;
;(define (union set1 set2)
;  (inner-union set1 (dedupe set2)))
;
;(define (inner-union set1 set2)
;  ; NOTE: This should be implemented as merge for efficiency.
;  (if (not (pair? set1))
;      set2
;      (insert (car set1) (inner-union (cdr set1) set2))))
;
;;; Remove consecutive duplicates from a list
;(define (dedupe lis)
;  (cond
;    ((null? lis) lis)
;    ((null? (cdr lis)) lis)
;    ((eq? (car lis) (cadr lis))
;     (dedupe (cdr lis)))
;    (else
;      (cons (car lis) (dedupe (cdr lis))))))
;
;(write (union '(a a a a b b c c c) '()))
;(write (union '(a b c) '(a a a a b b c c c) ))

(define (union set1 set2)
  ; NOTE: This should be implemented as merge for efficiency.
  (if (not (pair? set1))
      set2
      (insert 'todo #;(car set1) (union (cdr set1) set2))))

(write (union '(a b) '(c d)))
