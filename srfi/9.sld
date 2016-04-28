;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This is based on the implementation of SRFI 9 from chibi scheme.
;;;;
(define-library (srfi 9)
  (export 
    record?
    define-record-type
    register-simple-type
    make-type-predicate
    make-constructor
    make-getter
    make-setter
    slot-set!
    type-slot-offset
  )
  (import (scheme base)
          (scheme cyclone util))
  (begin
    (define record-marker (list 'record-marker))
    (define (register-simple-type name parent field-tags)
      (let ((new (make-vector 3 #f)))
        (vector-set! new 0 record-marker)
        (vector-set! new 1 name)
        (vector-set! new 2 field-tags)
        new))
    (define (make-type-predicate pred name)
      (lambda (obj)
        (and (vector? obj)
             (equal? (vector-ref obj 0) record-marker)
             (equal? (vector-ref obj 1) name))))
    (define (make-constructor make name)
      (lambda ()
        (let* ((field-tags (vector-ref name 2))
               (field-values (make-vector (length field-tags) #f))
               (new (make-vector 3 #f))
              )
          (vector-set! new 0 record-marker)
          (vector-set! new 1 name)
          (vector-set! new 2 field-values)
          new)))
    (define (type-slot-offset name sym)
      (let ((field-tags (vector-ref name 2)))
        (_list-index sym field-tags)))
    (define (slot-set! name obj idx val)
      (let ((vec obj)) ;; TODO: get actual slots from obj
        (vector-set! (vector-ref vec 2) idx val)))
    (define (make-getter sym name idx)
      (lambda (obj)
        (vector-ref (vector-ref obj 2) idx)))
    (define (make-setter sym name idx)
      (lambda (obj val)
        (vector-set! (vector-ref obj 2) idx val)))

    (define (record? obj)
      (and (vector? obj)
           (> (vector-length obj) 0)
           (equal? record-marker (vector-ref obj 0))))

    ;; Find index of element in list, or -1 if not found
    (define _list-index
      (lambda (e lst)
        (if (null? lst)
          -1
          (if (eq? (car lst) e)
            0
            (if (= (_list-index e (cdr lst)) -1) 
              -1
              (+ 1 (_list-index e (cdr lst))))))))

    (define-syntax define-record-type
      (er-macro-transformer
       (lambda (expr rename compare)
         (let* ((name+parent (cadr expr))
                (name (if (pair? name+parent) (car name+parent) name+parent))
                (parent (and (pair? name+parent) (cadr name+parent)))
                (name-str (symbol->string name)) ;(identifier->symbol name)))
                (procs (cddr expr))
                (make (caar procs))
                (make-fields (cdar procs))
                (pred (cadr procs))
                (fields (cddr procs))
                (_define (rename 'define))
                (_lambda (rename 'lambda))
                (_let (rename 'let))
                (_register (rename 'register-simple-type))
                (_slot-set! (rename 'slot-set!))
                (_type_slot_offset (rename 'type-slot-offset)))
           ;; catch a common mistake
           (if (eq? name make)
               (error "same binding for record rtd and constructor" name))
           `(,(rename 'begin)
             ;; type
             (,_define ,name (,_register 
                              ,name ;,name-str 
                              ,parent 
                              ',(map car fields)))
             ;; predicate
             (,_define ,pred (,(rename 'make-type-predicate)
                              ,pred ;(symbol->string pred) ;(identifier->symbol pred))
                              ,name))
             ;; fields
             ,@(map (lambda (f)
                      (and (pair? f) (pair? (cdr f))
                           `(,_define ,(cadr f)
                              (,(rename 'make-getter)
                               ,(symbol->string
                                 (cadr f)
                                 ;(identifier->symbol (cadr f))
                                )
                               ,name
                               (,_type_slot_offset ,name ',(car f))))))
                    fields)
             ,@(map (lambda (f)
                      (and (pair? f) (pair? (cdr f)) (pair? (cddr f))
                           `(,_define ,(car (cddr f))
                              (,(rename 'make-setter)
                               ,(symbol->string
                                 (car (cddr f))
                                 ;(identifier->symbol (car (cddr f)))
                                )
                               ,name
                               (,_type_slot_offset ,name ',(car f))))))
                    fields)
             ;; constructor
             (,_define ,make
               ,(let lp ((ls make-fields) (sets '()))
                  (cond
                   ((null? ls)
                    `(,_let ((%make (,(rename 'make-constructor)
                                     ,(symbol->string make) ;(identifier->symbol make))
                                     ,name)))
                       (,_lambda ,make-fields
                         (,_let ((res (%make)))
                           ,@sets
                           res))))
                   (else
                    (let ((field (assq (car ls) fields)))
                      (cond
                       ((not field)
                        (error "unknown record field in constructor" (car ls)))
                       ((pair? (cddr field))
                        (lp (cdr ls)
                            (cons `(,(car (cddr field)) res ,(car ls)) sets)))
                       (else
                        (lp (cdr ls)
                            (cons `(,_slot-set! ,name res (,_type_slot_offset ,name ',(car ls)) ,(car ls))
                                  sets)))))))))
      )
    ))))))
