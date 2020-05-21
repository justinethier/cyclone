;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains information about Cyclone's scheme primitives.
;;;;
(define-library (scheme cyclone primitives)
  (import (scheme base)
          (scheme cyclone hashset)
          ;(scheme write)
          (srfi 69)
  )
  (export
    prim?
    *primitives*
    *primitives-num-args*
    ;; TODO: replace w/list that cannot be precomputed: precompute-prim-app?
    prim-call?
    prim->c-func
    prim->c-func-uses-alloca?
    prim/data-arg?
    prim/c-var-pointer
    prim/c-var-assign
    prim/cvar?
    prim:inline-convert-prim-call
    prim:check-arg-count
    prim:mutates?
    prim:cont?
    prim:cont/no-args?
    prim:arg-count?
    prim:allocates-object?
    prim:immutable-args/result?
    ;; User defined function primitives
    ;*udf-prims*
    ;*udf-cps->inline*
    prim:udf?
    prim:add-udf!
    prim:func->prim
  )
  (begin
    (define *udf-prims* '())
    (define *udf-cps->inline* '())
    (define (prim:add-udf! cps-sym inline-sym)
      (set! *udf-cps->inline*
        (cons (cons cps-sym inline-sym) *udf-cps->inline*))
      (set! *udf-prims* (cons inline-sym *udf-prims*)))
    (define (prim:udf? exp)
      (memq exp *udf-prims*))

    (define *hs-prims* (hs-create))

    ; prim? : exp -> boolean
    (define (prim? exp)
      (or (hs-member? *hs-prims* exp)
          (memq exp *udf-prims*)))
    
    ;; Does primitive mutate any of its arguments?
    (define (prim:mutates? exp)
      (memq 
        exp
       '(
         apply
         Cyc-fast-apply
         Cyc-set-cvar!
         Cyc-spawn-thread!
         Cyc-end-thread!
         set-global!
         set-global-unsafe!
         set-cell!
         set-car!
         set-cdr!
         string-set!
         bytevector-u8-set!
         vector-set!)))

    (define *primitives* '(
         Cyc-global-vars
         Cyc-get-cvar
         Cyc-set-cvar!
         Cyc-cvar? ;; Cyclone-specific
         Cyc-opaque?
         Cyc-has-cycle?
         Cyc-spawn-thread!
         Cyc-end-thread!
         Cyc-stdout
         Cyc-stdin
         Cyc-stderr
         Cyc-list
         Cyc-if
         Cyc-foreign-code
         Cyc-foreign-value
         Cyc-fast-plus
         Cyc-fast-sub
         Cyc-fast-mul
         Cyc-fast-div
         +
         -
         *
         /
         Cyc-fast-eq
         Cyc-fast-gt
         Cyc-fast-lt
         Cyc-fast-gte
         Cyc-fast-lte
         Cyc-fast-char-eq
         Cyc-fast-char-gt
         Cyc-fast-char-lt
         Cyc-fast-char-gte
         Cyc-fast-char-lte
         =
         >
         <
         >=
         <=
         apply
         Cyc-fast-apply
         %halt
         exit
         system
         command-line-arguments
         Cyc-installation-dir
         Cyc-compilation-environment
         Cyc-default-exception-handler
         Cyc-current-exception-handler
         cons
         Cyc-fast-vector-2
         Cyc-fast-vector-3
         Cyc-fast-vector-4
         Cyc-fast-vector-5
         Cyc-fast-list-1
         Cyc-fast-list-2
         Cyc-fast-list-3
         Cyc-fast-list-4
         cell-get
         set-global!
         set-global-unsafe!
         set-cell!
         cell
         eq?
         eqv?
         equal?
         Cyc-fast-member
         Cyc-fast-assoc
         assv
         assq
         memq
         memv
         length
         set-car!
         set-cdr!
         car
         cdr
         caar cadr cdar cddr
         caaar caadr cadar caddr cdaar cdadr cddar cdddr
         caaaar caaadr caadar caaddr cadaar cadadr
         caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
         char->integer
         integer->char
         string->number
         string-append
         string-cmp
         list->string
         string->symbol
         symbol->string
         number->string
         string-length
         string-ref
         string-set!
         substring
         make-bytevector
         bytevector-length
         bytevector
         bytevector-append
         Cyc-bytevector-copy
         Cyc-utf8->string
         Cyc-string->utf8
         bytevector-u8-ref
         bytevector-u8-set!
         bytevector?
         make-vector
         list->vector
         vector-length
         vector-ref
         vector-set!
         boolean?
         char?
         eof-object?
         null?
         number?
         real?
         integer?
         pair?
         port?
         procedure?
         Cyc-macro?
         vector?
         string?
         symbol?
         open-input-file
         open-output-file
         open-binary-input-file
         open-binary-output-file
         close-port
         close-input-port
         close-output-port
         Cyc-flush-output-port
         file-exists?
         delete-file
         Cyc-read-char
         Cyc-peek-char
         Cyc-read-line
         Cyc-write-char
         Cyc-write
         Cyc-display
         Cyc-unsafe-car
         Cyc-unsafe-cdr
         ))

    ;; Keep track of how many args are required for each primitive.
    ;; For each primitive, define:
    ;;  - minimum number of args
    ;;  - maximum number of args
    ;; Normally these will be the same unless the function takes an
    ;; optional number of arguments. If a number is not 
    ;; applicable then it should be set to #f. If a primitive
    ;; can take any number of arguments then no validation is
    ;; required and the primitive does not need to be listed.
    (define *primitives-num-args* '(
         (Cyc-global-vars 0 0)
         (Cyc-get-cvar 1 1)
         (Cyc-set-cvar! 2 2)
         (Cyc-cvar? 1 1)
         (Cyc-opaque? 1 1)
         (Cyc-has-cycle? 1 1)
         (Cyc-spawn-thread! 1 1)
         (Cyc-end-thread! 0 0)
         (Cyc-stdout 0 0)
         (Cyc-stdin 0 0)
         (Cyc-stderr 0 0)
         (Cyc-if 3 3)
         (Cyc-foreign-code 1 #f)
         (Cyc-foreign-value 2 2)
         (Cyc-fast-plus 2 2)
         (Cyc-fast-sub 2 2)
         (Cyc-fast-mul 2 2)
         (Cyc-fast-div 2 2)
         (Cyc-fast-eq 2 2)
         (Cyc-fast-gt 2 2)
         (Cyc-fast-lt 2 2)
         (Cyc-fast-gte 2 2)
         (Cyc-fast-lte 2 2)
         (Cyc-fast-char-eq 2 2)
         (Cyc-fast-char-gt 2 2)
         (Cyc-fast-char-lt 2 2)
         (Cyc-fast-char-gte 2 2)
         (Cyc-fast-char-lte 2 2)
         (- 1 #f)
         (/ 1 #f)
         (= 2 #f)
         (> 2 #f)
         (< 2 #f)
         (>= 2 #f)
         (<= 2 #f)
         (apply 1 #f)
         (Cyc-fast-apply 2 #f)
         (%halt 1 1)
         (exit 1 1)
         (system 1 1)
         (command-line-arguments 0 0)
         (Cyc-installation-dir 1 1)
         (Cyc-compilation-environment 1 1)
         (Cyc-default-exception-handler 1 1)
         (Cyc-current-exception-handler 0 0)
         (cons 2 2)
         (Cyc-fast-vector-2 2 2)
         (Cyc-fast-vector-3 3 3)
         (Cyc-fast-vector-4 4 4)
         (Cyc-fast-vector-5 5 5)
         (Cyc-fast-list-1 1 1)
         (Cyc-fast-list-2 2 2)
         (Cyc-fast-list-3 3 3)
         (Cyc-fast-list-4 4 4)
         (cell-get 1 1)
         (set-global! 3 3)
         (set-global-unsafe! 3 3)
         (set-cell! 2 2)
         (cell 1 1)
         (eq? 2 2)
         (eqv? 2 2)
         (equal? 2 2)
         (Cyc-fast-member 2 2)
         (Cyc-fast-assoc 2 2)
         (assq 2 2)
         (assv 2 2)
         (memq 2 2)
         (memv 2 2)
         (length 1 1)
         (set-car! 2 2)
         (set-cdr! 2 2)
         (Cyc-unsafe-car 1 1)
         (Cyc-unsafe-cdr 1 1)
         (car 1 1)
         (cdr 1 1)
         (caar    1 1)
         (cadr    1 1)
         (cdar    1 1)
         (cddr    1 1)
         (caaar   1 1)
         (caadr   1 1)
         (cadar   1 1)
         (caddr   1 1)
         (cdaar   1 1)
         (cdadr   1 1)
         (cddar   1 1)
         (cdddr   1 1)
         (caaaar  1 1)
         (caaadr  1 1)
         (caadar  1 1)
         (caaddr  1 1)
         (cadaar  1 1)
         (cadadr  1 1)
         (caddar  1 1)
         (cadddr  1 1)
         (cdaaar  1 1)
         (cdaadr  1 1)
         (cdadar  1 1)
         (cdaddr  1 1)
         (cddaar  1 1)
         (cddadr  1 1)
         (cdddar  1 1)
         (cddddr  1 1)
         (char->integer 1 1)
         (integer->char 1 1)
         (string->number 1 2)
         (string-append #f #f)
         (string-cmp 2 2)
         (list->string 1 1)
         (string->symbol 1 1)
         (symbol->string 1 1)
         (number->string 1 2)
         (string-length 1 1)
         (string-ref 2 2)
         (string-set! 3 3)
         (substring 3 3)
         (make-bytevector 1 #f)
         (bytevector-length 1 1)
         (bytevector #f #f)
         (bytevector-append #f #f)
         (Cyc-bytevector-copy 3 3)
         (Cyc-utf8->string 3 3)
         (Cyc-string->utf8 3 3)
         (bytevector-u8-ref 2 2)
         (bytevector-u8-set! 3 3)
         (bytevector? 1 1)
         (make-vector 1 #f)
         (list->vector 1 1)
         (vector-length 1 1)
         (vector-ref 2 2)
         (vector-set! 3 3)
         (boolean? 1 1)
         (char? 1 1)
         (eof-object? 1 1)
         (null? 1 1)
         (number? 1 1)
         (real? 1 1)
         (integer? 1 1)
         (pair? 1 1)
         (port? 1 1)
         (procedure? 1 1)
         (Cyc-macro? 1 1)
         (vector? 1 1)
         (string? 1 1)
         (symbol? 1 1)
         (open-input-file 1 1)
         (open-output-file 1 1)
         (open-binary-input-file 1 1)
         (open-binary-output-file 1 1)
         (close-port 1 1)
         (close-input-port 1 1)
         (close-output-port 1 1)
         (Cyc-flush-output-port 1 1)
         (file-exists? 1 1)
         (delete-file 1 1)
         (Cyc-read-char 1 1)
         (Cyc-peek-char 1 1)
         (Cyc-read-line 1 1)
         (Cyc-write-char 1 2)
         (Cyc-write 1 2)
         (Cyc-display 1 2)
    ))

    ;; Return #f the primitive cannot accept the given number of
    ;; arguments, and #t otherwise.
    (define (prim:check-arg-count sym num-args expected)
      (let ((build-error-str 
              (lambda (prefix expected actual)
                (string-append
                  prefix
                  (number->string expected)
                  " arguments to " 
                  (symbol->string sym)
                  " but received "
                  (number->string actual)
                ))))
        (cond
          ((not expected) #t)
          ((and (not (null? (cdr expected)))
                (cadr expected)
                (> num-args (cadr expected)))
           (error (build-error-str "Expected " (car expected) num-args)))
          ((and (car expected)
                (< num-args (car expected)))
           (error (build-error-str "Expected at least " (car expected) num-args)))
          (else #t))))
      

;; TODO: dont' put this here, just the list
;    ;; Constant Folding
;    ;; Is a primitive being applied in such a way that it can be
;    ;; evaluated at compile time?
;    (define (precompute-prim-app? ast)
;      (and 
;        (pair? ast)
;        (prim? (car ast))
;        ;; Does not make sense to precompute these
;        (not (memq (car ast)
;                    '(Cyc-global-vars
;                      Cyc-get-cvar
;                      Cyc-set-cvar!
;                      Cyc-cvar?
;                      Cyc-opaque?
;                      Cyc-spawn-thread!
;                      Cyc-end-thread!
;                      apply
;                      %halt
;                      exit
;                      system
;                      command-line-arguments
;                      Cyc-installation-dir
;                      Cyc-default-exception-handler
;                      Cyc-current-exception-handler
;                      cell-get
;                      set-global!
;                      set-cell!
;                      cell
;                      cons
;                      set-car!
;                      set-cdr!
;                      string-set!
;                      string->symbol ;; Could be mistaken for an identifier
;                      make-bytevector
;                      make-vector
;                      ;; I/O must be done at runtime for side effects:
;                      Cyc-stdout
;                      Cyc-stdin
;                      Cyc-stderr
;                      open-input-file
;                      open-output-file
;                      close-port
;                      close-input-port
;                      close-output-port
;                      Cyc-flush-output-port
;                      file-exists?
;                      delete-file
;                      Cyc-read-char
;                      Cyc-peek-char
;                      Cyc-read-line
;                      Cyc-write-char
;                      Cyc-write
;                      Cyc-display)))
;        (call/cc
;          (lambda (return)
;            (for-each
;              (lambda (expr)
;                (if (or (vector? expr)
;                        (not (const? expr)))
;                  (return #f)))
;              (cdr ast))
;            #t))))

    (define (prim-call? exp)
      (and (list? exp) (prim? (car exp))))

    (define (prim->c-func-uses-alloca? p use-alloca?)
      (and
        use-alloca?
        (member 
           p 
          '(;cons
            ;Cyc-fast-list-1
            ;Cyc-fast-list-2
            ;Cyc-fast-list-3
            ;Cyc-fast-list-4
            ;cell
           ))))

;; TODO: get rid of this function and replace this with the same type of pre-alloc that
;;       we do for fast numeric operations. That will allow us to prevent out-of-order 
;;       execution for these as part of Cyc-seq
    (define (prim->c-func p use-alloca? emit-unsafe)
      (cond
         (use-alloca?
          ;; Special case, when this flag is set the compiler is requesting a
          ;; primitive that will allocate data, so any new objects must be
          ;; created via alloca or such, and cannot be declared as stack vars.
          ;; This is to support C loops in place of recursion.
          (cond
            ;((eq? p 'cons)            "alloca_pair")
            ;((eq? p 'Cyc-fast-list-1) "alloca_list_1")
            ;((eq? p 'Cyc-fast-list-2) "alloca_list_2")
            ;((eq? p 'Cyc-fast-list-3) "alloca_list_3")
            ;((eq? p 'Cyc-fast-list-4) "alloca_list_4")
            ;((eq? p 'cell)            "alloca_cell")
            (else
              (_prim->c-func p emit-unsafe))))
         (else
           (_prim->c-func p emit-unsafe))))

    (define (_prim->c-func p emit-unsafe)
      (cond
         ((eq? p 'Cyc-global-vars)       "Cyc_get_global_variables")
         ((eq? p 'Cyc-get-cvar)          "Cyc_get_cvar")
         ((eq? p 'Cyc-set-cvar!)         "Cyc_set_cvar")
         ((eq? p 'Cyc-cvar?)             "Cyc_is_cvar")
         ((eq? p 'Cyc-opaque?)           "Cyc_is_opaque")
         ((eq? p 'Cyc-has-cycle?)        "Cyc_has_cycle")
         ((eq? p 'Cyc-spawn-thread!)     "Cyc_spawn_thread")
         ((eq? p 'Cyc-end-thread!)       "Cyc_end_thread")
         ((eq? p 'Cyc-stdout)            "Cyc_stdout")
         ((eq? p 'Cyc-stdin)             "Cyc_stdin")
         ((eq? p 'Cyc-stderr)            "Cyc_stderr")
         ((eq? p 'Cyc-list)              "Cyc_list")
         ((eq? p 'Cyc-if)                "Cyc_if")
         ((eq? p 'Cyc-foreign-code)      "UNDEF")
         ((eq? p 'Cyc-foreign-value)     "UNDEF")
         ((eq? p 'Cyc-fast-plus)         "Cyc_fast_sum")
         ((eq? p 'Cyc-fast-sub)          "Cyc_fast_sub")
         ((eq? p 'Cyc-fast-mul)          "Cyc_fast_mul")
         ((eq? p 'Cyc-fast-div)          "Cyc_fast_div")
         ((eq? p '+)                     "Cyc_sum")
         ((eq? p '-)                     "Cyc_sub")
         ((eq? p '*)                     "Cyc_mul")
         ((eq? p '/)                     "Cyc_div")
         ((eq? p 'Cyc-fast-eq)           "Cyc_num_fast_eq_op")
         ((eq? p 'Cyc-fast-gt)           "Cyc_num_fast_gt_op")
         ((eq? p 'Cyc-fast-lt)           "Cyc_num_fast_lt_op")
         ((eq? p 'Cyc-fast-gte)          "Cyc_num_fast_gte_op")
         ((eq? p 'Cyc-fast-lte)          "Cyc_num_fast_lte_op")
         ((eq? p 'Cyc-fast-char-eq)      "Cyc_char_eq_op")
         ((eq? p 'Cyc-fast-char-gt)      "Cyc_char_gt_op")
         ((eq? p 'Cyc-fast-char-lt)      "Cyc_char_lt_op")
         ((eq? p 'Cyc-fast-char-gte)     "Cyc_char_gte_op")
         ((eq? p 'Cyc-fast-char-lte)     "Cyc_char_lte_op")
         ((eq? p '=)                     "Cyc_num_eq")
         ((eq? p '>)                     "Cyc_num_gt")
         ((eq? p '<)                     "Cyc_num_lt")
         ((eq? p '>=)                    "Cyc_num_gte")
         ((eq? p '<=)                    "Cyc_num_lte")
         ((eq? p 'apply)                 "apply_va")
         ((eq? p 'Cyc-fast-apply)        "apply")
         ((eq? p '%halt)                 "__halt")
         ((eq? p 'exit)                  "__halt")
         ((eq? p 'Cyc-default-exception-handler)  "Cyc_default_exception_handler")
         ((eq? p 'Cyc-current-exception-handler)  "Cyc_current_exception_handler")
         ((eq? p 'open-input-file)       "Cyc_io_open_input_file")
         ((eq? p 'open-output-file)      "Cyc_io_open_output_file")
         ((eq? p 'open-binary-input-file)       "Cyc_io_open_binary_input_file")
         ((eq? p 'open-binary-output-file)      "Cyc_io_open_binary_output_file")
         ((eq? p 'close-port)            "Cyc_io_close_port")
         ((eq? p 'close-input-port)      "Cyc_io_close_input_port")
         ((eq? p 'close-output-port)     "Cyc_io_close_output_port")
         ((eq? p 'Cyc-flush-output-port) "Cyc_io_flush_output_port")
         ((eq? p 'file-exists?)          "Cyc_io_file_exists")
         ((eq? p 'delete-file)           "Cyc_io_delete_file")
         ((eq? p 'Cyc-read-char)             "Cyc_io_read_char")
         ((eq? p 'Cyc-peek-char)             "Cyc_io_peek_char")
         ((eq? p 'Cyc-read-line)         "Cyc_io_read_line")
         ((eq? p 'Cyc-display)           "Cyc_display_va")
         ((eq? p 'Cyc-write)             "Cyc_write_va")
         ((eq? p 'Cyc-write-char)        "Cyc_write_char")
         ((eq? p 'Cyc-unsafe-car)        "car")
         ((eq? p 'Cyc-unsafe-cdr)        "cdr")
         ((eq? p 'car)
          (if emit-unsafe
              "Cyc_car_unsafe"
              "Cyc_car"))
         ((eq? p 'cdr)
          (if emit-unsafe
              "Cyc_cdr_unsafe"
              "Cyc_cdr"))
         ((eq? p 'caar)          "Cyc_caar")
         ((eq? p 'cadr)          "Cyc_cadr")
         ((eq? p 'cdar)          "Cyc_cdar")
         ((eq? p 'cddr)          "Cyc_cddr")
         ((eq? p 'caaar)         "Cyc_caaar")
         ((eq? p 'caadr)         "Cyc_caadr")
         ((eq? p 'cadar)         "Cyc_cadar")
         ((eq? p 'caddr)         "Cyc_caddr")
         ((eq? p 'cdaar)         "Cyc_cdaar")
         ((eq? p 'cdadr)         "Cyc_cdadr")
         ((eq? p 'cddar)         "Cyc_cddar")
         ((eq? p 'cdddr)         "Cyc_cdddr")
         ((eq? p 'caaaar)        "Cyc_caaaar")
         ((eq? p 'caaadr)        "Cyc_caaadr")
         ((eq? p 'caadar)        "Cyc_caadar")
         ((eq? p 'caaddr)        "Cyc_caaddr")
         ((eq? p 'cadaar)        "Cyc_cadaar")
         ((eq? p 'cadadr)        "Cyc_cadadr")
         ((eq? p 'caddar)        "Cyc_caddar")
         ((eq? p 'cadddr)        "Cyc_cadddr")
         ((eq? p 'cdaaar)        "Cyc_cdaaar")
         ((eq? p 'cdaadr)        "Cyc_cdaadr")
         ((eq? p 'cdadar)        "Cyc_cdadar")
         ((eq? p 'cdaddr)        "Cyc_cdaddr")
         ((eq? p 'cddaar)        "Cyc_cddaar")
         ((eq? p 'cddadr)        "Cyc_cddadr")
         ((eq? p 'cdddar)        "Cyc_cdddar")
         ((eq? p 'cddddr)        "Cyc_cddddr")
         ((eq? p 'char->integer) "Cyc_char2integer")
         ((eq? p 'integer->char) "Cyc_integer2char")
         ((eq? p 'string->number)"Cyc_string2number2_")
         ((eq? p 'list->string)  "Cyc_list2string")
         ((eq? p 'make-bytevector)   "Cyc_make_bytevector")
         ((eq? p 'bytevector-length) "Cyc_bytevector_length")
         ((eq? p 'bytevector)        "Cyc_bytevector")
         ((eq? p 'bytevector-append)    "Cyc_bytevector_append")
         ((eq? p 'Cyc-bytevector-copy)    "Cyc_bytevector_copy")
         ((eq? p 'Cyc-utf8->string)    "Cyc_utf82string")
         ((eq? p 'Cyc-string->utf8)    "Cyc_string2utf8")
         ((eq? p 'bytevector-u8-ref)    "Cyc_bytevector_u8_ref")
         ((eq? p 'bytevector-u8-set!)   "Cyc_bytevector_u8_set")
         ((eq? p 'make-vector)   "Cyc_make_vector")
         ((eq? p 'list->vector)  "Cyc_list2vector")
         ((eq? p 'vector-length) "Cyc_vector_length")
         ((eq? p 'vector-ref)   
          (if emit-unsafe
              "Cyc_vector_ref_unsafe"
              "Cyc_vector_ref"))
         ((eq? p 'vector-set!)  
          (if emit-unsafe
              "Cyc_vector_set_unsafe_cps"
              "Cyc_vector_set_cps"))
         ((eq? p 'string-append) "Cyc_string_append")
         ((eq? p 'string-cmp)    "Cyc_string_cmp")
         ((eq? p 'string->symbol) "Cyc_string2symbol")
         ((eq? p 'symbol->string) "Cyc_symbol2string")
         ((eq? p 'number->string) "Cyc_number2string2")
         ((eq? p 'string-length)  "Cyc_string_length")
         ((eq? p 'string-ref)     "Cyc_string_ref")
         ((eq? p 'string-set!)    "Cyc_string_set")
         ((eq? p 'substring)      "Cyc_substring")
         ((eq? p 'Cyc-installation-dir) "Cyc_installation_dir")
         ((eq? p 'Cyc-compilation-environment) "Cyc_compilation_environment")
         ((eq? p 'command-line-arguments) "Cyc_command_line_arguments")
         ((eq? p 'system)         "Cyc_system")
         ((eq? p 'length)        
          (if emit-unsafe
              "Cyc_length_unsafe"
              "Cyc_length"))
         ((eq? p 'set-car!)      "Cyc_set_car_cps")
         ((eq? p 'set-cdr!)      "Cyc_set_cdr_cps")
         ((eq? p 'eq?)           "Cyc_eq")
         ((eq? p 'eqv?)          "Cyc_eqv")
         ((eq? p 'equal?)        "equalp")
         ((eq? p 'Cyc-fast-member)  "memberp")
         ((eq? p 'Cyc-fast-assoc)   "assoc")
         ((eq? p 'assq)          "assq")
         ((eq? p 'assv)          "assq")
         ((eq? p 'memq)          "memqp")
         ((eq? p 'memv)          "memqp")
         ((eq? p 'boolean?)      "Cyc_is_boolean")
         ((eq? p 'char?)         "Cyc_is_char")
         ((eq? p 'null?)         "Cyc_is_null")
         ((eq? p 'number?)       "Cyc_is_number")
         ((eq? p 'real?)         "Cyc_is_real")
         ((eq? p 'integer?)      "Cyc_is_integer")
         ((eq? p 'pair?)         "Cyc_is_pair")
         ((eq? p 'procedure?)    "Cyc_is_procedure")
         ((eq? p 'Cyc-macro?)    "Cyc_is_macro")
         ((eq? p 'port?)         "Cyc_is_port")
         ((eq? p 'vector?)       "Cyc_is_vector")
         ((eq? p 'bytevector?)   "Cyc_is_bytevector")
         ((eq? p 'string?)       "Cyc_is_string")
         ((eq? p 'eof-object?)   "Cyc_is_eof_object")
         ((eq? p 'symbol?)       "Cyc_is_symbol")
         ((eq? p 'cons)          "set_pair_as_expr")
         ((eq? p 'Cyc-fast-vector-2) "Cyc_fast_vector_2")
         ((eq? p 'Cyc-fast-vector-3) "Cyc_fast_vector_3")
         ((eq? p 'Cyc-fast-vector-4) "Cyc_fast_vector_4")
         ((eq? p 'Cyc-fast-vector-5) "Cyc_fast_vector_5")
         ((eq? p 'Cyc-fast-list-1) "set_cell_as_expr")
         ((eq? p 'Cyc-fast-list-2) "Cyc_fast_list_2")
         ((eq? p 'Cyc-fast-list-3) "Cyc_fast_list_3")
         ((eq? p 'Cyc-fast-list-4) "Cyc_fast_list_4")
         ((eq? p 'cell)          "set_cell_as_expr")
         ((eq? p 'cell-get)      "car") ;; Unsafe as cell gets added by compiler
         ((eq? p 'set-cell!)     "Cyc_set_cell")
         ((eq? p 'set-global!)   "global_set_cps_id")
         ((eq? p 'set-global-unsafe!)   "global_set_id")
         (else
           (error "unhandled primitive: " p))))

    ;; Does the primitive require passing thread data as its first argument?
    (define (prim/data-arg? p)
     (or
      (memq p '(
        Cyc-list
        Cyc-foreign-code
        Cyc-foreign-value
        Cyc-fast-plus
        Cyc-fast-sub
        Cyc-fast-mul
        Cyc-fast-div
        +
        -
        *
        /
        Cyc-fast-eq
        Cyc-fast-gt
        Cyc-fast-lt
        Cyc-fast-gte
        Cyc-fast-lte
        Cyc-fast-char-eq
        Cyc-fast-char-gt
        Cyc-fast-char-lt
        Cyc-fast-char-gte
        Cyc-fast-char-lte
        =
        >
        <
        >=
        <=
        Cyc-fast-member
        Cyc-fast-assoc
        Cyc-fast-apply
        apply
        car
        cdr
        caar cadr cdar cddr
        caaar caadr cadar caddr cdaar cdadr cddar cdddr
        caaaar caaadr caadar caaddr cadaar cadadr
        caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
        Cyc-default-exception-handler
        Cyc-current-exception-handler
        Cyc-end-thread!
        open-input-file
        open-output-file
        open-binary-input-file
        open-binary-output-file
        close-port
        close-input-port
        close-output-port
        Cyc-flush-output-port
        Cyc-display
        Cyc-write
        file-exists?
        delete-file
        Cyc-read-char
        Cyc-peek-char
        Cyc-read-line
        Cyc-write-char
        integer->char
        string->number
        list->string
        make-bytevector
        bytevector-length
        bytevector-append
        Cyc-bytevector-copy
        Cyc-utf8->string
        Cyc-string->utf8
        bytevector
        bytevector-u8-ref
        bytevector-u8-set!
        make-vector
        list->vector
        vector-length
        vector-ref
        vector-set!
        string-append
        string-cmp
        string->symbol
        symbol->string
        number->string
        string-length
        string-ref
        string-set!
        substring
        Cyc-installation-dir
        Cyc-compilation-environment
        command-line-arguments
        assq
        assv
        ;assoc
        memq
        memv
        ;member
        length
        set-car!
        set-cdr!
        procedure?
        set-cell!
        set-global!))
       (memq p *udf-prims*)))

    ;; Determine if primitive receives a pointer to a local C variable
    (define (prim/c-var-pointer p)
      (cond
        ((eq? p 'cons) "pair_type")
        ((eq? p 'cell) "pair_type")
        ((eq? p 'Cyc-fast-vector-2) "vector_2_type")
        ((eq? p 'Cyc-fast-vector-3) "vector_3_type")
        ((eq? p 'Cyc-fast-vector-4) "vector_4_type")
        ((eq? p 'Cyc-fast-vector-5) "vector_5_type")
        ((eq? p 'Cyc-fast-list-1) "pair_type")
        ((eq? p 'Cyc-fast-list-2) "list_2_type")
        ((eq? p 'Cyc-fast-list-3) "list_3_type")
        ((eq? p 'Cyc-fast-list-4) "list_4_type")
        ((eq? p 'Cyc-fast-plus) "complex_num_type")
        ((eq? p 'Cyc-fast-sub) "complex_num_type")
        ((eq? p 'Cyc-fast-mul) "complex_num_type")
        ((eq? p 'Cyc-fast-div) "complex_num_type")
        ((memq p *udf-prims*) "complex_num_type")
        (else #f)))

;; TODO: this only makes sense for macros, all functions need to be removed from here.
;;       longer-term we need to fix issues with these functions, Cyc-seq, and the
;;       possibility of out-of-order execution due to prims being evaluated at the
;;       C declaration instead of in the body of the function
;; TODO: does make sense for conts, those can't be in Cyc-seq. OK to keep those here
    ;; Determine if primitive assigns (allocates) a C variable
    ;; EG: int v = prim();
    (define (prim/c-var-assign p)
      (cond
        ((eq? p 'Cyc-stdout) "port_type")
        ((eq? p 'Cyc-stdin) "port_type")
        ((eq? p 'Cyc-stderr) "port_type")
        ((eq? p 'open-input-file) "port_type")
        ((eq? p 'open-output-file) "port_type")
        ((eq? p 'open-binary-input-file) "port_type")
        ((eq? p 'open-binary-output-file) "port_type")
        ;((eq? p 'Cyc-fast-plus) "object")
        ;((eq? p 'Cyc-fast-sub) "object")
        ;((eq? p 'Cyc-fast-mul) "object")
        ;;((eq? p 'Cyc-fast-div) "object")
        ((eq? p '+) "object")
        ((eq? p '-) "object")
        ((eq? p '*) "object")
        ((eq? p '/) "object")
        ;((eq? p 'Cyc-fast-eq) "object")
        ;((eq? p 'Cyc-fast-gt) "object")
        ;((eq? p 'Cyc-fast-lt) "object")
        ;((eq? p 'Cyc-fast-gte) "object")
        ;((eq? p 'Cyc-fast-lte) "object")
        ;((eq? p 'Cyc-fast-char-eq) "object")
        ;((eq? p 'Cyc-fast-char-gt) "object")
        ;((eq? p 'Cyc-fast-char-lt) "object")
        ;((eq? p 'Cyc-fast-char-gte) "object")
        ;((eq? p 'Cyc-fast-char-lte) "object")
        ((eq? p '=) "object")
        ((eq? p '>) "object")
        ((eq? p '<) "object")
        ((eq? p '>=) "object")
        ((eq? p '<=) "object")
        ((eq? p 'string->number) "object")
        ((eq? p 'string-append) "object")
        ((eq? p 'apply)  "object")
        ((eq? p 'Cyc-fast-apply)  "object")
        ((eq? p 'Cyc-read-line) "object")
        ((eq? p 'Cyc-read-char) "object")
        ((eq? p 'Cyc-peek-char) "object")
        ((eq? p 'command-line-arguments) "object")
        ((eq? p 'number->string) "object")
        ((eq? p 'symbol->string) "object")
        ((eq? p 'substring) "object")
        ((eq? p 'make-bytevector) "object")
        ((eq? p 'bytevector) "object")
        ((eq? p 'bytevector-append) "object")
        ((eq? p 'Cyc-bytevector-copy) "object")
        ((eq? p 'Cyc-utf8->string) "object")
        ((eq? p 'Cyc-string->utf8) "object")
        ((eq? p 'make-vector) "object")
        ((eq? p 'list->string) "object")
        ((eq? p 'list->vector) "object")
        ((eq? p 'set-car!) "object")
        ((eq? p 'set-cdr!) "object")
        ((eq? p 'vector-set!) "object")
        ((eq? p 'set-global!) "object")
        ((eq? p 'Cyc-installation-dir) "object")
        ((eq? p 'Cyc-compilation-environment) "object")
        ;((memq p *udf-prims*) "object")
        (else #f)))

    ;; Determine if primitive creates a C variable
    (define (prim/cvar? exp)
        (and (prim? exp)
             (or
               (memq exp '(
                 Cyc-stdout
                 Cyc-stdin
                 Cyc-stderr
                 open-input-file
                 open-output-file
                 open-binary-input-file
                 open-binary-output-file
                 Cyc-installation-dir
                 Cyc-compilation-environment
                 string->number 
                 string-append list->string
                 make-bytevector
                 bytevector
                 bytevector-append
                 Cyc-bytevector-copy
                 Cyc-utf8->string
                 Cyc-string->utf8
                 make-vector list->vector
                 symbol->string number->string 
                 substring
                 set-car!
                 set-cdr!
                 vector-set!
                 set-global!
                 ;Cyc-fast-plus
                 ;Cyc-fast-sub
                 ;Cyc-fast-mul
                 ;Cyc-fast-div
                 ;Cyc-fast-eq
                 ;Cyc-fast-gt
                 ;Cyc-fast-lt
                 ;Cyc-fast-gte
                 ;Cyc-fast-lte
                 ;Cyc-fast-char-eq
                 ;Cyc-fast-char-gt
                 ;Cyc-fast-char-lt
                 ;Cyc-fast-char-gte
                 ;Cyc-fast-char-lte
                 + - * / 
                 apply 
                 Cyc-fast-apply
                 = > < >= <=
                 command-line-arguments
                 Cyc-read-line
                 Cyc-read-char Cyc-peek-char
                 ;Cyc-fast-list-1
                 ;Cyc-fast-list-2
                 ;Cyc-fast-list-3
                 ;Cyc-fast-list-4
                 ;cons
                 ;cell
                ))
               ;(memq exp *udf-prims*)
               )))

    ;; Pass continuation as the function's first parameter?
    (define (prim:cont? exp)
      (and (prim? exp)
           (memq exp '(Cyc-read-line apply command-line-arguments number->string 
                         Cyc-fast-apply
                         + - * /
                         = > < >= <=
                         set-car!
                         set-cdr!
                         vector-set!
                         set-global!
                         Cyc-list
                         Cyc-read-char Cyc-peek-char 
                         symbol->string list->string substring string-append string->number
                         make-bytevector
                         bytevector-append
                         Cyc-bytevector-copy
                         Cyc-utf8->string
                         Cyc-string->utf8
                         bytevector
                         bytevector-u8-ref
                         bytevector-u8-set!
                         make-vector 
                         list->vector 
                         Cyc-compilation-environment
                         Cyc-installation-dir))))

    ;; Primitive functions that pass a continuation or thread data but have no other arguments
    (define (prim:cont/no-args? exp)
      (and (prim? exp)
           (memq exp '(command-line-arguments Cyc-current-exception-handler))))

    ;; Pass an integer arg count as the function's first parameter?
    (define (prim:arg-count? exp)
        (and (prim? exp)
             (memq exp '(error Cyc-write Cyc-display 
                           number->string string->number string-append 
                           apply
                           make-bytevector
                           bytevector
                           bytevector-append
                           make-vector
                           Cyc-list
                           = > < >= <=
                           + - * /))))

    ;; Does primitive allocate an object?
    ;; TODO: these are the functions that are defined via macros. This method
    ;; is obsolete and should be replaced by prim:cont? functions over time.
    (define (prim:allocates-object? exp use-alloca?)
        (and  (prim? exp)
              use-alloca?
              (memq exp 
                '(
                ;cons
                ))))
    
    ;; Does the primitive only accept/return immutable objects?
    ;; This is useful during optimization
    (define (prim:immutable-args/result? sym)
      (memq sym 
             '(= > < >= <=
               + - * /
               Cyc-fast-plus
               Cyc-fast-sub
               Cyc-fast-mul
               Cyc-fast-div
               Cyc-fast-eq
               Cyc-fast-gt
               Cyc-fast-lt
               Cyc-fast-gte
               Cyc-fast-lte
               Cyc-fast-char-eq
               Cyc-fast-char-gt
               Cyc-fast-char-lt
               Cyc-fast-char-gte
               Cyc-fast-char-lte
;              %halt
;              exit
               char->integer
               integer->char
               ;; Are these OK? If obj is mutated the prim will still work because
               ;; the type information will not change.
               Cyc-cvar?
               Cyc-opaque?
               boolean?
               char?
               eof-object?
               null?
               number?
               real?
               integer?
               pair?
               port?
               procedure?
               Cyc-macro?
               vector?
               string?
               symbol?
    )))

    (define (prim:inline-convert-prim-call prim-call)
      (cond
        ((and (equal? (car prim-call) '+) (= (length prim-call) 2)) `(Cyc-fast-plus 0 ,@(cdr prim-call)))
        ((and (equal? (car prim-call) '*) (= (length prim-call) 2)) `(Cyc-fast-mul 1 ,@(cdr prim-call)))
        ((equal? (car prim-call) '+) (->dyadic (cons 'Cyc-fast-plus (cdr prim-call))))
        ((equal? (car prim-call) '*) (->dyadic (cons 'Cyc-fast-mul (cdr prim-call))))
        ((and (equal? (car prim-call) '-) (= (length prim-call) 2)) `(Cyc-fast-sub 0 ,@(cdr prim-call))) ;; Special case, fast negation
        ((equal? (car prim-call) '-) (->dyadic (cons 'Cyc-fast-sub (cdr prim-call))))
        ((and (equal? (car prim-call) '/) (= (length prim-call) 2)) `(Cyc-fast-div 1 ,@(cdr prim-call))) ;; Special case, fast inversion
        ((equal? (car prim-call) '/) (->dyadic (cons 'Cyc-fast-div (cdr prim-call))))
        ((and (equal? (car prim-call) '=) (= (length prim-call) 3))  (cons 'Cyc-fast-eq (cdr prim-call)))
        ((and (equal? (car prim-call) '>) (= (length prim-call) 3))  (cons 'Cyc-fast-gt (cdr prim-call)))
        ((and (equal? (car prim-call) '<) (= (length prim-call) 3))  (cons 'Cyc-fast-lt (cdr prim-call)))
        ((and (equal? (car prim-call) '>=) (= (length prim-call) 3)) (cons 'Cyc-fast-gte (cdr prim-call)))
        ((and (equal? (car prim-call) '<=) (= (length prim-call) 3)) (cons 'Cyc-fast-lte (cdr prim-call)))
        ((and (equal? (car prim-call) 'apply) (= (length prim-call) 3)) (cons 'Cyc-fast-apply (cdr prim-call)))
        (else
         prim-call)))

    ;; Take an expression containing a single function call and break it up
    ;; into many calls of 2 arguments each.
    (define (->dyadic expr)
      (cond
        ((< (length expr) 4)
         expr)
        (else
         (let ((fnc (car expr)))
           (foldl
             (lambda (x acc)
               (list fnc acc x))
             `(,fnc ,(cadr expr) ,(caddr expr))
             (cdddr expr))))))

  ;; Map from a Scheme function to a primitive, if possible.
  ;;
  ;; Inputs:
  ;; - Function symbol
  ;; - Number of arguments to the function
  ;;
  ;; Note the only reason to do this is to change from a CPS-style
  ;; function to one that can be inlined with no CPS, which yields
  ;; a significant speed improvement. Or, alternatively there can 
  ;; also be a speed improvement if the prim can avoid having to 
  ;; load C varargs.
  (define (prim:func->prim func-sym num-args)
    (define mappings
      '(
         (char=?  2 Cyc-fast-char-eq )
         (char>?  2 Cyc-fast-char-gt )
         (char<?  2 Cyc-fast-char-lt )
         (char>=? 2 Cyc-fast-char-gte)
         (char<=? 2 Cyc-fast-char-lte)
         (read-char 1 Cyc-read-char)
         (peek-char 1 Cyc-peek-char)
       ))
    (let ((m (assoc func-sym mappings))
          (udf (assoc func-sym *udf-cps->inline*)) 
         )
      (cond
        ;; Upgrade to a primitive using hardcoded mappings
        ((and m (= (cadr m) num-args)) 
         (caddr m)) 
        ;; Upgrade to a (non-CPS) Scheme function
        (udf ;; for now do not check args
         (cdr udf))
        ;; No match; keep original function
        (else func-sym)))) 

  (hs-add-all! *hs-prims* *primitives*)
))
