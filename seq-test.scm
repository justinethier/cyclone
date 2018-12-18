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

(write (insert 'a '(c d b a a a a))) (newline)
(write (insert 'a '(c d b a a a a))) (newline)
(write (insert 'a '())) (newline)
(write (insert 'a '(b c))) (newline)
(write (insert 'a '(a b c))) (newline)
(newline)
(write (fast-insert 'a '(c d b a a a a))) (newline)
(write (fast-insert 'a '(c d b a a a a))) (newline)
(write (fast-insert 'a '())) (newline)
(write (fast-insert 'a '(b c))) (newline)
(write (fast-insert 'a '(a b c))) (newline)

(define (insert sym S)
  (if (not (pair? S))
      (list sym)
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        ;(else (insert sym (cdr S))))))
        (else (cons (car S) (insert sym (cdr S)))))))

(define-c fast-insert
 "(void *data, int argc, closure _,object k_7318, object sym_731_7312, object S_732_7313)"
 "
 pair_type *acc = NULL, *acc_tail = NULL;
 object result;
 while(1) {
  if( (boolean_f != Cyc_is_pair(S_732_7313)) ){ 
    if( (boolean_f != Cyc_eq(sym_731_7312, Cyc_car(data, S_732_7313))) ){ 
      //return_closcall1(data,  k_7318,  S_732_7313);
      result = S_732_7313;
      break;
    } else { 
      if (strcmp(symbol_desc(sym_731_7312), 
                 symbol_desc(Cyc_car(data, S_732_7313))) < 0) {
      //complex_num_type local_7350; 
      //if( (boolean_f != ((inline_function_type)
      //                   ((closure)__glo_symbol_121_127_191_191inline_191_191)->fn)(data,&local_7350, sym_731_7312, Cyc_car(data, S_732_7313))) ){ 
        //pair_type local_7356; 
        //return_closcall1(data,  k_7318,  set_pair_as_expr(&local_7356, sym_731_7312, S_732_7313));
        pair_type* local_7356 = alloca(sizeof(pair_type));
        set_pair(local_7356, sym_731_7312, S_732_7313);
        result = local_7356;
        break;
      } else { 
        pair_type *p = alloca(sizeof(pair_type));
        set_pair(p, Cyc_car(data, S_732_7313), NULL);
        if (acc == NULL) {
          acc = p;
          acc_tail = acc;
        } else {
          cdr(acc_tail) = p;
          acc_tail = p;
        }
        S_732_7313 = Cyc_cdr(data, S_732_7313);
        continue;
      }
    }
  } else { 
    //pair_type local_7363; 
    //return_closcall1(data,  k_7318,  set_cell_as_expr(&local_7363, sym_731_7312));
    pair_type *local_7363 = alloca(sizeof(pair_type));
    set_pair(local_7363, sym_731_7312, NULL);
    result = local_7363;
    break;
  }
}

if (acc) {
  cdr(acc_tail) = result;
  return_closcall1(data, k_7318, (object)acc);
} else {
  return_closcall1(data, k_7318, result);
}
")

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
;
;(define (union set1 set2)
;  ; NOTE: This should be implemented as merge for efficiency.
;  (if (not (pair? set1))
;      set2
;      (insert 'todo #;(car set1) (union (cdr set1) set2))))
;
;(write (union '(a b) '(c d)))

(define (union l1 l2)
  (inner-union #f l1 l2))

(define inner-union
  (lambda (last l1 l2)
    (if (null? l1)
        (next last l2)
        (if (null? l2)
            (next last l1)
            ;; TODO: also have an eq? check to eliminate duplicates
            (if (symbol<? (car l1) (car l2))
                (if (eq? last (car l1))
                    (inner-union last (cdr l1) l2)
                    (cons (car l1) (inner-union (car l1) (cdr l1) l2)))
                (if (eq? last (car l2))
                    (inner-union last l1 (cdr l2))
                    (cons (car l2) (inner-union (car l2) l1 (cdr l2)))))))))

(define (next sym lis)
  (if (and (pair? lis) (eq? sym (car lis)))
      (next sym (cdr lis))
      lis))

(write
  (union '(a b c) '(d e f)))
(newline)
(write
  (union '(p) '(p)))
