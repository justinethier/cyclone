;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains information about Cyclone's scheme primitives.
;;;;
(define-library (scheme cyclone primitives)
  (import (scheme base))
  (export
    prim?
    *primitives*
    *primitives-num-args*
    ;; TODO: replace w/list that cannot be precomputed: precompute-prim-app?
    prim-call?
    prim->c-func
    prim/data-arg?
    prim/c-var-assign
    prim/cvar?
    prim:check-arg-count
    prim:mutates?
    prim:cont?
    prim:cont/no-args?
    prim:arg-count?
    prim:allocates-object?)
  (begin
    ; prim? : exp -> boolean
    (define (prim? exp)
      (member exp *primitives*))
    
    ;; Does primitive mutate any of its arguments?
    (define (prim:mutates? exp)
      (member 
        exp
       '(
         Cyc-set-cvar!
         Cyc-spawn-thread!
         Cyc-end-thread!
         set-global!
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
         +
         -
         *
         /
         =
         >
         <
         >=
         <=
         apply
         %halt
         exit
         system
         command-line-arguments
         Cyc-installation-dir
         Cyc-default-exception-handler
         Cyc-current-exception-handler
         cons
         cell-get
         set-global!
         set-cell!
         cell
         eq?
         eqv?
         equal?
         assoc
         assq
         assv
         memq
         memv
         member
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
         macro?
         vector?
         string?
         symbol?
         open-input-file
         open-output-file
         close-port
         close-input-port
         close-output-port
         Cyc-flush-output-port
         file-exists?
         delete-file
         read-char
         peek-char
         Cyc-read-line
         Cyc-write-char
         Cyc-write
         Cyc-display))

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
;         (Cyc-global-vars
;         (Cyc-get-cvar
;         (Cyc-set-cvar!
;         (Cyc-cvar?
;         (Cyc-opaque?
;         (Cyc-has-cycle?
;         (Cyc-spawn-thread!
;         (Cyc-end-thread!
;         (Cyc-stdout
;         (Cyc-stdin
;         (Cyc-stderr
;         (+
         (- 1 #f)
;         (*
;         (/
;         (=
;         (>
;         (<
;         (>=
;         (<=
;         (apply
;         (%halt
;         (exit
;         (system
;         (command-line-arguments
;         (Cyc-installation-dir
;         (Cyc-default-exception-handler
;         (Cyc-current-exception-handler
;         (cons
;         (cell-get
;         (set-global!
;         (set-cell!
;         (cell
;         (eq?
;         (eqv?
;         (equal?
;         (assoc
;         (assq
;         (assv
;         (memq
;         (memv
;         (member
;         (length
;         (set-car!
;         (set-cdr!
;         (car
;         (cdr
;         (caar 
;         (cadr 
;         (cdar 
;         (cddr
;         (caaar 
;         (caadr 
;         (cadar 
;         (caddr 
;         (cdaar 
;         (cdadr 
;         (cddar 
;         (cdddr
;         (caaaar 
;         (caaadr 
;         (caadar 
;         (caaddr 
;         (cadaar 
;         (cadadr
;         (caddar 
;         (cadddr 
;         (cdaaar 
;         (cdaadr 
;         (cdadar 
;         (cdaddr 
;         (cddaar 
;         (cddadr 
;         (cdddar 
;         (cddddr
;         (char->integer
;         (integer->char
;         (string->number
;         (string-append
;         (string-cmp
;         (list->string
;         (string->symbol
;         (symbol->string
;         (number->string
;         (string-length
;         (string-ref
;         (string-set!
;         (substring
;         (make-bytevector
;         (bytevector-length
;         (bytevector
;         (bytevector-append
;         (Cyc-bytevector-copy
;         (Cyc-utf8->string
;         (Cyc-string->utf8
;         (bytevector-u8-ref
;         (bytevector-u8-set!
;         (bytevector?
;         (make-vector
;         (list->vector
;         (vector-length
;         (vector-ref
;         (vector-set!
;         (boolean?
;         (char?
;         (eof-object?
;         (null?
;         (number?
;         (real?
;         (integer?
;         (pair?
;         (port?
;         (procedure?
;         (macro?
;         (vector?
;         (string?
;         (symbol?
;         (open-input-file
;         (open-output-file
;         (close-port
;         (close-input-port
;         (close-output-port
;         (Cyc-flush-output-port
;         (file-exists?
;         (delete-file
;         (read-char
;         (peek-char
;         (Cyc-read-line
;         (Cyc-write-char
;         (Cyc-write
;         (Cyc-display
    ))

    ;; Return #f the primitive cannot accept the given number of
    ;; arguments, and #t otherwise.
    (define (prim:check-arg-count sym num-args expected)
      (let ((build-error-str 
              (lambda (prefix expected actual)
                (string-append
                  prefix
                  (number->string expected)
                  " args but received "
                  (number->string actual)
                ))))
        (cond
          ((not expected) #t)
          ((and (car expected)
                (> num-args (car expected)))
           (error (build-error-str "Expected " (car expected) num-args) sym))
          ((and (not (null? (cdr expected)))
                (cadr expected)
                (< num-args (cadr expected)))
           (error (build-error-str "Expected at least " (car expected) num-args) sym))
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
;        (not (member (car ast)
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
;                      read-char
;                      peek-char
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

    (define (prim->c-func p)
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
         ((eq? p '+)                     "Cyc_sum")
         ((eq? p '-)                     "Cyc_sub")
         ((eq? p '*)                     "Cyc_mul")
         ((eq? p '/)                     "Cyc_div")
         ((eq? p '=)                     "Cyc_num_eq")
         ((eq? p '>)                     "Cyc_num_gt")
         ((eq? p '<)                     "Cyc_num_lt")
         ((eq? p '>=)                    "Cyc_num_gte")
         ((eq? p '<=)                    "Cyc_num_lte")
         ((eq? p 'apply)                 "apply_va")
         ((eq? p '%halt)                 "__halt")
         ((eq? p 'exit)                  "__halt")
         ((eq? p 'Cyc-default-exception-handler)  "Cyc_default_exception_handler")
         ((eq? p 'Cyc-current-exception-handler)  "Cyc_current_exception_handler")
         ((eq? p 'open-input-file)       "Cyc_io_open_input_file")
         ((eq? p 'open-output-file)      "Cyc_io_open_output_file")
         ((eq? p 'close-port)            "Cyc_io_close_port")
         ((eq? p 'close-input-port)      "Cyc_io_close_input_port")
         ((eq? p 'close-output-port)     "Cyc_io_close_output_port")
         ((eq? p 'Cyc-flush-output-port) "Cyc_io_flush_output_port")
         ((eq? p 'file-exists?)          "Cyc_io_file_exists")
         ((eq? p 'delete-file)           "Cyc_io_delete_file")
         ((eq? p 'read-char)             "Cyc_io_read_char")
         ((eq? p 'peek-char)             "Cyc_io_peek_char")
         ((eq? p 'Cyc-read-line)         "Cyc_io_read_line")
         ((eq? p 'Cyc-display)           "Cyc_display_va")
         ((eq? p 'Cyc-write)             "Cyc_write_va")
         ((eq? p 'Cyc-write-char)        "Cyc_write_char")
         ((eq? p 'car)           "car")
         ((eq? p 'cdr)           "cdr")
         ((eq? p 'caar)          "caar")
         ((eq? p 'cadr)          "cadr")
         ((eq? p 'cdar)          "cdar")
         ((eq? p 'cddr)          "cddr")
         ((eq? p 'caaar)         "caaar")
         ((eq? p 'caadr)         "caadr")
         ((eq? p 'cadar)         "cadar")
         ((eq? p 'caddr)         "caddr")
         ((eq? p 'cdaar)         "cdaar")
         ((eq? p 'cdadr)         "cdadr")
         ((eq? p 'cddar)         "cddar")
         ((eq? p 'cdddr)         "cdddr")
         ((eq? p 'caaaar)        "caaaar")
         ((eq? p 'caaadr)        "caaadr")
         ((eq? p 'caadar)        "caadar")
         ((eq? p 'caaddr)        "caaddr")
         ((eq? p 'cadaar)        "cadaar")
         ((eq? p 'cadadr)        "cadadr")
         ((eq? p 'caddar)        "caddar")
         ((eq? p 'cadddr)        "cadddr")
         ((eq? p 'cdaaar)        "cdaaar")
         ((eq? p 'cdaadr)        "cdaadr")
         ((eq? p 'cdadar)        "cdadar")
         ((eq? p 'cdaddr)        "cdaddr")
         ((eq? p 'cddaar)        "cddaar")
         ((eq? p 'cddadr)        "cddadr")
         ((eq? p 'cdddar)        "cdddar")
         ((eq? p 'cddddr)        "cddddr")
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
         ((eq? p 'vector-ref)    "Cyc_vector_ref")
         ((eq? p 'vector-set!)   "Cyc_vector_set")
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
         ((eq? p 'command-line-arguments) "Cyc_command_line_arguments")
         ((eq? p 'system)         "Cyc_system")
         ((eq? p 'assq)          "assq")
         ((eq? p 'assv)          "assq")
         ((eq? p 'assoc)         "assoc")
         ((eq? p 'memq)          "memqp")
         ((eq? p 'memv)          "memqp")
         ((eq? p 'member)        "memberp")
         ((eq? p 'length)        "Cyc_length")
         ((eq? p 'set-car!)      "Cyc_set_car")
         ((eq? p 'set-cdr!)      "Cyc_set_cdr")
         ((eq? p 'eq?)           "Cyc_eq")
         ((eq? p 'eqv?)          "Cyc_eq")
         ((eq? p 'equal?)        "equalp")
         ((eq? p 'boolean?)      "Cyc_is_boolean")
         ((eq? p 'char?)         "Cyc_is_char")
         ((eq? p 'null?)         "Cyc_is_null")
         ((eq? p 'number?)       "Cyc_is_number")
         ((eq? p 'real?)         "Cyc_is_real")
         ((eq? p 'integer?)      "Cyc_is_integer")
         ((eq? p 'pair?)         "Cyc_is_pair")
         ((eq? p 'procedure?)    "Cyc_is_procedure")
         ((eq? p 'macro?)        "Cyc_is_macro")
         ((eq? p 'port?)         "Cyc_is_port")
         ((eq? p 'vector?)       "Cyc_is_vector")
         ((eq? p 'bytevector?)   "Cyc_is_bytevector")
         ((eq? p 'string?)       "Cyc_is_string")
         ((eq? p 'eof-object?)   "Cyc_is_eof_object")
         ((eq? p 'symbol?)       "Cyc_is_symbol")
         ((eq? p 'cons)          "make_pair")
         ((eq? p 'cell)          "make_cell")
         ((eq? p 'cell-get)      "cell_get")
         ((eq? p 'set-cell!)     "Cyc_set_car")
         ((eq? p 'set-global!)   "global_set")
         (else
           (error "unhandled primitive: " p))))

    ;; Does the primitive require passing thread data as its first argument?
    (define (prim/data-arg? p)
      (member p '(
        +
        -
        *
        /
        =
        >
        <
        >=
        <=
        apply
        Cyc-default-exception-handler
        Cyc-current-exception-handler
        Cyc-end-thread!
        open-input-file
        open-output-file
        close-port
        close-input-port
        close-output-port
        Cyc-flush-output-port
        file-exists?
        delete-file
        read-char
        peek-char
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
        command-line-arguments
        assq
        assv
        assoc
        memq
        memv
        member
        length
        set-car!
        set-cdr!
        procedure?
        set-cell!)))

    ;; Determine if primitive assigns (allocates) a C variable
    ;; EG: int v = prim();
    (define (prim/c-var-assign p)
      (cond
        ((eq? p 'Cyc-stdout) "port_type")
        ((eq? p 'Cyc-stdin) "port_type")
        ((eq? p 'Cyc-stderr) "port_type")
        ((eq? p 'open-input-file) "port_type")
        ((eq? p 'open-output-file) "port_type")
        ((eq? p '+) "object")
        ((eq? p '-) "object")
        ((eq? p '*) "object")
        ((eq? p '/) "object")
        ((eq? p '=) "object")
        ((eq? p '>) "object")
        ((eq? p '<) "object")
        ((eq? p '>=) "object")
        ((eq? p '<=) "object")
        ((eq? p 'string->number) "object")
        ((eq? p 'string-append) "object")
        ((eq? p 'apply)  "object")
        ((eq? p 'Cyc-read-line) "object")
        ((eq? p 'read-char) "object")
        ((eq? p 'peek-char) "object")
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
        ((eq? p 'Cyc-installation-dir) "object")
        (else #f)))

    ;; Determine if primitive creates a C variable
    (define (prim/cvar? exp)
        (and (prim? exp)
             (member exp '(
                 Cyc-stdout
                 Cyc-stdin
                 Cyc-stderr
                 open-input-file
                 open-output-file
                 Cyc-installation-dir
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
                 + - * / apply 
                 = > < >= <=
                 command-line-arguments
                 Cyc-read-line
                 read-char peek-char
                 cons cell))))

    ;; Pass continuation as the function's first parameter?
    (define (prim:cont? exp)
      (and (prim? exp)
           (member exp '(Cyc-read-line apply command-line-arguments number->string 
                         + - * /
                         = > < >= <=
                         read-char peek-char 
                         symbol->string list->string substring string-append string->number
                         make-bytevector
                         bytevector-append
                         Cyc-bytevector-copy
                         Cyc-utf8->string
                         Cyc-string->utf8
                         bytevector
                         bytevector-u8-ref
                         bytevector-u8-set!
                         make-vector list->vector Cyc-installation-dir))))

    ;; Primitive functions that pass a continuation or thread data but have no other arguments
    (define (prim:cont/no-args? exp)
      (and (prim? exp)
           (member exp '(command-line-arguments Cyc-current-exception-handler))))

    ;; Pass an integer arg count as the function's first parameter?
    (define (prim:arg-count? exp)
        (and (prim? exp)
             (member exp '(error Cyc-write Cyc-display 
                           number->string string->number string-append 
                           apply
                           make-bytevector
                           bytevector
                           bytevector-append
                           make-vector
                           = > < >= <=
                           + - * /))))

    ;; Does primitive allocate an object?
    ;; TODO: these are the functions that are defined via macros. This method
    ;; is obsolete and should be replaced by prim:cont? functions over time.
    (define (prim:allocates-object? exp)
        (and  (prim? exp)
              (member exp '())))
))
