;;
;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module compiles scheme code to a Cheney-on-the-MTA C runtime.
;;

(define (emit line)
  (display line)
  (newline))

(define (emits str)
  (display str))

(define (emit-newline)
  (newline))

(define (string-join lst delim)
  (cond
    ((null? lst) 
      "")
    ((= (length lst) 1) 
      (car lst))
    (else
      (string-append 
        (car lst) 
        delim 
        (string-join (cdr lst) delim)))))

;; Escape chars in a C-string, so it can be safely written to a C file
(define (cstr:escape-chars str)
  (letrec ((next (lambda (head tail)
                   (cond
                     ((null? head) (list->string (reverse tail)))
                     ((equal? (car head) #\")
                      (next (cdr head) (cons #\" (cons #\\ tail))))
                     ((equal? (car head) #\\)
                      (next (cdr head) (cons #\\ (cons #\\ tail))))
                     ((equal? (car head) #\newline)
                      (next (cdr head) 
                            (cons #\n (cons #\\ tail))))
                     (else
                       (next (cdr head) (cons (car head) tail)))))))
    (next (string->list str) '())))

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

(define *c-main-function*
"main(int argc,char **argv)
{long stack_size = long_arg(argc,argv,\"-s\",STACK_SIZE);
 long heap_size = long_arg(argc,argv,\"-h\",HEAP_SIZE);
 global_stack_size = stack_size;
 global_heap_size = heap_size;
 main_main(stack_size,heap_size,(char *) &stack_size);
 return 0;}")

;;; Auto-generation of C macros
(define *c-call-max-args* 128)
(define *c-call-arity* (make-vector (+ 1 *c-call-max-args*) #f))

(define (set-c-call-arity! arity)
  (cond
    ((not (number? arity))
     (error `(Non-numeric number of arguments received ,arity)))
    ((> arity *c-call-max-args*)
     (error "Only support up to 128 arguments. Received: " arity))
    (else
      (vector-set! *c-call-arity* arity #t))))

(define (emit-c-arity-macros arity)
  (when (<= arity *c-call-max-args*)
    (cond
      ((or (= arity 1) (= arity 2)
           (vector-ref *c-call-arity* arity))
       (emit (c-macro-funcall arity))
       (emit (c-macro-return-funcall arity))
       (emit (c-macro-return-check arity))))
    (emit-c-arity-macros (+ arity 1))))

(define (c-macro-return-funcall num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
      "/* Return to continuation after checking for stack overflow. */\n"
      "#define return_funcall" n "(cfn" args ") \\\n"
      "{char stack; \\\n"
      " if (check_overflow(&stack,stack_limit1)) { \\\n"
      "     object buf[" n "]; " arry-assign "\\\n"
      "     GC(cfn,buf," n "); return; \\\n"
      " } else {funcall" n "((closure) (cfn)" args "); return;}}\n")))

(define (c-macro-return-check num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
      "/* Evaluate an expression after checking for stack overflow. */\n"
      "#define return_check" n "(_fn" args ") { \\\n"
      " char stack; \\\n"
      " if (check_overflow(&stack,stack_limit1)) { \\\n"
      "     object buf[" n "]; " arry-assign " \\\n"
      "     mclosure0(c1, _fn); \\\n"
      "     GC(&c1, buf, " n "); return; \\\n"
      " } else { (_fn)(" n ",(closure)_fn" args "); }}\n")))

(define (c-macro-funcall num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (n-1 (number->string (if (> num-args 0) (- num-args 1) 0)))
        (wrap (lambda (s) (if (> num-args 0) s ""))))
    (string-append
      "#define funcall" n "(cfn" args ") "
        (wrap (string-append "if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(" n-1 ", (closure)a1, cfn" (if (> num-args 1) (substring args 3 (string-length args)) "") "); }"))
        (wrap " else { ")
        "((cfn)->fn)(" n ",cfn" args ")"
        (wrap ";}")
        )))

(define (c-macro-n-prefix n prefix)
  (if (> n 0)
    (string-append
      (c-macro-n-prefix (- n 1) prefix)
      (string-append prefix (number->string n)))
    ""))

(define (c-macro-array-assign n prefix assign)
  (if (> n 0)
    (string-append
      (c-macro-array-assign (- n 1) prefix assign)
      prefix "[" (number->string (- n 1)) "] = " 
      assign (number->string n) ";")
    ""))

(define (c-macro-GC-globals)
  ; emit directly to be more efficient
  ; TODO: convert all c-macro functions to direct emit???
  (for-each
    (lambda (global)
      (emits "\n  add_global((object *) &")
      (emits (mangle-global (car global)))
      (emits ");"))
    *globals*)
  (emit ""))

(define (c-macro-declare-globals)
  (for-each
      (lambda (global)
        (emits "object ")
        (emits (mangle-global (car global)))
        (emits " = nil;\n"))
      *globals*)
  (emit "")
  (emit ""))

;;; Compilation routines.

;; Return generated code that also requests allocation of C variables on stack
(define (c-code/vars str cvars)
  (list str
        cvars))

;; Return generated code with no C variables allocated on the stack
(define (c-code str) (c-code/vars str (list)))

;; Append arg count to a C code pair
(define (c:tuple/args cp num-args)
  (append cp (list num-args)))

;; Functions to work with data structures that contain C code:
;;
;; body - The actual body of C code
;; allocs - Allocations made by C code, eg "int c"
;; num-args - Number of function arguments combined in the tuple (optional)
;;
(define (c:body c-pair) (car c-pair))
(define (c:allocs c-pair) (cadr c-pair))
(define (c:num-args c-tuple) (caddr c-tuple))

(define (c:allocs->str c-allocs . prefix)
  (apply
    string-append
    (map
        (lambda (c)
           (string-append 
            (if (null? prefix)
                ""
                (car prefix))
            c 
            "\n"))
        c-allocs)))

(define (c:allocs->str2 c-allocs prefix suffix)
  (apply
    string-append
    (map
        (lambda (c)
           (string-append prefix c suffix))
        c-allocs)))

(define (c:append cp1 cp2)
  (c-code/vars 
    (string-append (c:body cp1) (c:body cp2))
    (append (c:allocs cp1) (c:allocs cp2))))

(define (c:append/prefix prefix cp1 cp2)
  (c-code/vars 
    (string-append prefix (c:body cp1) (c:body cp2))
    (append (c:allocs cp1) (c:allocs cp2))))

(define (c:serialize cp prefix)
    (string-append
        (c:allocs->str (c:allocs cp) prefix)
        prefix
        (c:body cp)))

;; c-compile-program : exp -> string
(define (c-compile-program exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble "cont")))
    ;(write `(DEBUG ,body))
    (string-append 
     preamble 
     (c:serialize body "  ") ;" ;\n"
;     "int main (int argc, char* argv[]) {\n"
;     "  return 0;\n"
;     " }\n"
)))

;; c-compile-exp : exp (string -> void) -> string
;;
;; exp - expression to compiler
;; append-preamble - ??
;; cont - name of the next continuation
;;        this is experimental and probably needs refinement
(define (c-compile-exp exp append-preamble cont)
  (cond
    ; Core forms:
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       
     ;; TODO: this needs to be more refined, probably w/a lookup table
     (c-code (string-append "primitive_" (mangle exp))))
    ((ref?   exp)       (c-compile-ref exp))
    ((quote? exp)       (c-compile-quote exp))
    ((if? exp)          (c-compile-if exp append-preamble cont))

    ; IR (2):
    ((tagged-list? '%closure exp)
     (c-compile-closure exp append-preamble cont))
    ; Global definition
    ((define? exp)
     (c-compile-global exp append-preamble cont))
    ; Special case - global function w/out a closure. Create an empty closure
    ((tagged-list? 'lambda exp)
     (c-compile-exp
      `(%closure ,exp)
       append-preamble cont))
    
    ; Application:      
    ((app? exp)         (c-compile-app exp append-preamble cont))
    (else               (error "unknown exp in c-compile-exp: " exp))))

(define (c-compile-quote qexp)
  (let ((exp (cadr qexp)))
    (c-compile-scalars exp)))

(define (c-compile-scalars args)
  (letrec (
    (num-args 0)
    (create-cons
      (lambda (cvar a b)
        (c-code/vars
          (string-append "make_cons(" cvar "," (c:body a) "," (c:body b) ");")
          (append (c:allocs a) (c:allocs b))))
    )
    (_c-compile-scalars 
     (lambda (args)
       (cond
        ((null? args)
           (c-code "nil"))
        ((not (pair? args))
         (c-compile-const args))
        (else
           (let* ((cvar-name (mangle (gensym 'c)))
                  (cell (create-cons
                          cvar-name
                          (c-compile-const (car args)) 
                          (_c-compile-scalars (cdr args)))))
             (set! num-args (+ 1 num-args))
             (c-code/vars
                (string-append "&" cvar-name)
                (append
                  (c:allocs cell)
                  (list (c:body cell))))))))))
  (c:tuple/args
    (_c-compile-scalars args) 
    num-args)))

;; c-compile-const : const-exp -> c-pair
;;
;; Typically this function is used to compile constant values such as
;; a single number, boolean, etc. However, it can be passed a quoted
;; item such as a list, to compile as a literal.
(define (c-compile-const exp)
  (cond
    ((null? exp)
     (c-code "nil"))
    ((pair? exp)
     (c-compile-scalars exp))
    ((integer? exp) 
      (let ((cvar-name (mangle (gensym 'c))))
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list     ; Allocate integer on the C stack
              (string-append 
                "make_int(" cvar-name ", " (number->string exp) ");")))))
    ((real? exp)
      (let ((cvar-name (mangle (gensym 'c))))
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list     ; Allocate on the C stack
              (string-append 
                "make_double(" cvar-name ", " (number->string exp) ");")))))
    ((boolean? exp) 
      (c-code (string-append
                (if exp "boolean_t" "boolean_f"))))
    ((char? exp)
     (c-code (string-append "obj_char2obj(" 
               (number->string (char->integer exp)) ")")))
    ((string? exp)
      (let ((cvar-name (mangle (gensym 'c))))
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list     ; Allocate integer on the C stack
              (string-append 
                "make_string(" cvar-name ", " (->cstr exp) ");")))))
;TODO: not good enough, need to store new symbols in a table so they can
;be inserted into the C program
    ((symbol? exp)
     (allocate-symbol exp)
     (c-code (string-append "quote_" (mangle exp))))
    (else
      (error "unknown constant: " exp))))

;; Convert a "scheme" string to a corresponding representation in C.
;; Keep in mind scheme strings can span lines, contain chars that
;; might not be allowed in C, etc.
(define (->cstr str) 
  (string-append "\"" (cstr:escape-chars str) "\""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives

(define (prim->c-func p)
  (cond
     ((eq? p 'Cyc-global-vars)       "Cyc_get_global_variables")
     ((eq? p 'Cyc-get-cvar)          "Cyc_get_cvar")
     ((eq? p 'Cyc-set-cvar!)         "Cyc_set_cvar")
     ((eq? p 'Cyc-cvar?)             "Cyc_is_cvar")
     ((eq? p 'Cyc-has-cycle?)        "Cyc_has_cycle")
     ((eq? p '+)                     "Cyc_sum")
     ((eq? p '-)                     "Cyc_sub")
     ((eq? p '*)                     "Cyc_mul")
     ((eq? p '/)                     "Cyc_div")
     ((eq? p '=)                     "__num_eq")
     ((eq? p '>)                     "__num_gt")
     ((eq? p '<)                     "__num_lt")
     ((eq? p '>=)                    "__num_gte")
     ((eq? p '<=)                    "__num_lte")
     ((eq? p 'apply)                 "apply")
     ((eq? p '%halt)                 "__halt")
     ((eq? p 'exit)                  "__halt")
     ((eq? p 'Cyc-default-exception-handler)  "Cyc_default_exception_handler")
     ((eq? p 'Cyc-current-exception-handler)  "Cyc_current_exception_handler")
     ((eq? p 'current-input-port)    "Cyc_io_current_input_port")
     ((eq? p 'open-input-file)       "Cyc_io_open_input_file")
     ((eq? p 'close-input-port)      "Cyc_io_close_input_port")
     ((eq? p 'read-char)             "Cyc_io_read_char")
     ((eq? p 'peek-char)             "Cyc_io_peek_char")
     ((eq? p 'display)               "Cyc_display")
     ((eq? p 'write)                 "Cyc_write")
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
     ((eq? p 'string->number)"Cyc_string2number")
     ((eq? p 'list->string)  "Cyc_list2string")
     ((eq? p 'string->list)  "string2list")
     ((eq? p 'string-append) "Cyc_string_append")
     ((eq? p 'string->symbol) "Cyc_string2symbol")
     ((eq? p 'symbol->string) "Cyc_symbol2string")
     ((eq? p 'number->string) "Cyc_number2string")
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
     ((eq? p 'pair?)         "Cyc_is_cons")
     ((eq? p 'procedure?)    "Cyc_is_procedure")
     ((eq? p 'string?)       "Cyc_is_string")
     ((eq? p 'eof-object?)   "Cyc_is_eof_object")
     ((eq? p 'symbol?)       "Cyc_is_symbol")
     ((eq? p 'cons)          "make_cons")
     ((eq? p 'cell)          "make_cell")
     ((eq? p 'cell-get)      "cell_get")
     ((eq? p 'set-cell!)     "Cyc_set_car")
     ((eq? p 'set-global!)   "global_set")
     (else
       (error "unhandled primitive: " p))))

;; Determine if primitive assigns (allocates) a C variable
;; EG: int v = prim();
(define (prim/c-var-assign p)
  (cond
    ((eq? p 'current-input-port) "port_type")
    ((eq? p 'open-input-file) "port_type")
    ((eq? p 'length) "integer_type")
    ((eq? p 'char->integer) "integer_type")
    ((eq? p '+) "common_type")
    ((eq? p '-) "common_type")
    ((eq? p '*) "common_type")
    ((eq? p '/) "common_type")
    ((eq? p 'string->number) "common_type")
    ((eq? p 'list->string) "string_type")
;    ((eq? p 'string->list) "object")
    ((eq? p 'string-append) "string_type")
    ((eq? p 'symbol->string) "string_type")
    ((eq? p 'number->string) "string_type")
    ((eq? p 'apply)  "object")
    (else #f)))

;; Determine if primitive creates a C variable
(define (prim/cvar? exp)
    (and (prim? exp)
         (member exp '(
             current-input-port open-input-file
             char->integer string->number string-append list->string string->list
             symbol->string number->string
             + - * / apply cons length cell))))

;; Pass an integer arg count as the function's first parameter?
(define (prim:arg-count? exp)
    (and (prim? exp)
         (member exp '(error string-append + - * /))))

;; Does primitive allocate an object?
(define (prim:allocates-object? exp)
    (and  (prim? exp)
          (member exp '(string->list))))

;; c-compile-prim : prim-exp -> string -> string
(define (c-compile-prim p cont)
  (let* ((c-func (prim->c-func p))
         ;; Following closure defs are only used for apply, to
         ;; create a new closure for the continuation, if needed.
         ;;
         ;; Apply is different in that it takes a continuation so that it can
         ;; allocate arbitrary data as needed using alloca, and then call into
         ;; the cont so allocations can remain on stack until GC.
         (closure-sym (mangle (gensym 'c)))
         (closure-def
           (cond
             ((and (eq? p 'apply)
                   (> (string-length cont) (string-length "__lambda_"))
                   (equal? (substring cont 0 9) "__lambda_"))
              (string-append 
                "mclosure0(" closure-sym 
                "," cont "); "))
             (else #f)))
         ;; END apply defs
         (c-var-assign 
            (lambda (type)
              (let ((cv-name (mangle (gensym 'c))))
                (c-code/vars 
                  (string-append (if (eq? p 'apply) "" "&") cv-name)
                  (list
                    (string-append 
                      ;; Define closure if necessary (apply only)
                      (cond
                       (closure-def closure-def)
                       (else ""))

                      ;; Emit C variable
                      type " " cv-name " = " c-func "("

                      ;; Emit closure as first arg, if necessary (apply only)
                      (cond
                       (closure-def
                        (string-append "&" closure-sym ", "))
                       ((eq? p 'apply) 
                        (string-append cont ", "))
                       (else "")))))))))
    (cond
     ((prim/c-var-assign p)
      (c-var-assign (prim/c-var-assign p)))
     ((prim/cvar? p)
        (let ((cv-name (mangle (gensym 'c))))
           (c-code/vars 
            (if (prim:allocates-object? p)
                cv-name ;; Already a pointer
                (string-append "&" cv-name)) ;; Point to data
            (list
                (string-append c-func "(" cv-name)))))
     (else
        (c-code (string-append c-func "("))))))

;; END primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; c-compile-ref : ref-exp -> string
(define (c-compile-ref exp)
  (c-code 
    (if (member exp *global-syms*)
      (mangle-global exp)
      (mangle exp))))

; c-compile-args : list[exp] (string -> void) -> string
(define (c-compile-args args append-preamble prefix cont)
  (letrec ((num-args 0)
         (_c-compile-args 
          (lambda (args append-preamble prefix cont)
            (if (not (pair? args))
                (c-code "")
                (begin
                  ;(trace:debug `(c-compile-args ,(car args)))
                  (set! num-args (+ 1 num-args))
                  (c:append/prefix
                    prefix 
                    (c-compile-exp (car args) 
                      append-preamble cont)
                    (_c-compile-args (cdr args) 
                      append-preamble ", " cont)))))))
  (c:tuple/args
    (_c-compile-args args 
      append-preamble prefix cont)
    num-args)))

;; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble cont)
  ;(trace:debug `(c-compile-app: ,exp))
  (let (($tmp (mangle (gensym 'tmp))))
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
      (cond
        ((lambda? fun)
         (let* ((lid (allocate-lambda (c-compile-lambda fun))) ;; TODO: pass in free vars? may be needed to track closures
                                                               ;; properly, wait until this comes up in an example
                (this-cont (string-append "__lambda_" (number->string lid)))
                (cgen 
                  (c-compile-args
                     args 
                     append-preamble 
                     ""
                     this-cont))
                (num-cargs (c:num-args cgen)))
           (set-c-call-arity! num-cargs)
           (c-code
             (string-append
               (c:allocs->str (c:allocs cgen))
               "return_check" (number->string num-cargs) 
               "(" this-cont
               (if (> num-cargs 0) "," "") ; TODO: how to propagate continuation - cont " "
                  (c:body cgen) ");"))))

        ((prim? fun)
         (let* ((c-fun 
                 (c-compile-prim fun cont))
                (c-args
                 (c-compile-args args append-preamble "" ""))
                (num-args (length args))
                (num-args-str
                  (string-append 
                    (number->string num-args)
                    (if (> num-args 0) "," "")))
                (c-args* (if (prim:arg-count? fun)
                             (c:append (c-code num-args-str) c-args)
                             c-args)))
            (if (prim/cvar? fun)
              ;; Args need to go with alloc function
              (c-code/vars
                (c:body c-fun)
                (append
                  (c:allocs c-args*) ;; fun alloc depends upon arg allocs
                  (list (string-append 
                    (car (c:allocs c-fun)) 
                         (if (prim/c-var-assign fun) "" ",") ; Allocating C var
                         (c:body c-args*) ");"))))
              ;; Args stay with body
              (c:append
                (c:append c-fun c-args*)
                (c-code ")")))))

        ((equal? '%closure-ref fun)
         (c-code (apply string-append (list
            "("
            ;; TODO: probably not the ideal solution, but works for now
            "(closureN)"
            (mangle (car args))
            ")->elts["
            (number->string (- (cadr args) 1))"]"))))

        ;; TODO: may not be good enough, closure app could be from an elt
        ((tagged-list? '%closure-ref fun)
         (let* ((cfun (c-compile-args (list (car args)) append-preamble "  " cont))
                (this-cont (c:body cfun))
                (cargs (c-compile-args (cdr args) append-preamble "  " this-cont)))
           (set-c-call-arity! (c:num-args cargs))
           (c-code 
             (string-append
               (c:allocs->str (c:allocs cfun) "\n")
               (c:allocs->str (c:allocs cargs) "\n")
               "return_funcall" (number->string (c:num-args cargs))
               "("
               this-cont
               (if (> (c:num-args cargs) 0) "," "")
               (c:body cargs)
               ");"))))

        ((tagged-list? '%closure fun)
         (let* ((cfun (c-compile-closure 
                        fun append-preamble cont))
                (this-cont (string-append "(closure)" (c:body cfun)))
                (cargs (c-compile-args
                         args append-preamble "  " this-cont))
                (num-cargs (c:num-args cargs)))
           (set-c-call-arity! num-cargs)
           (c-code
             (string-append
                (c:allocs->str (c:allocs cfun) "\n")
                (c:allocs->str (c:allocs cargs) "\n")
                "return_funcall" (number->string num-cargs)
                "("
                this-cont
                (if (> num-cargs 0) "," "")
                (c:body cargs)
                ");"))))

        (else
         (error `(Unsupported function application ,exp)))))))

; c-compile-if : if-exp -> string
(define (c-compile-if exp append-preamble cont)
  (let* ((compile (lambda (exp)
                    (c-compile-exp exp append-preamble cont)))
         (test (compile (if->condition exp)))
         (then (compile (if->then exp)))
         (els (compile (if->else exp))))
  (c-code (string-append
   (c:allocs->str (c:allocs test) "  ")
   "if( !eq(boolean_f, "
   (c:body test)
   ") ){ \n"
   (c:serialize then "  ")
   "\n} else { \n"
   (c:serialize els "  ")
   "}\n"))))

;; Global compilation
(define *globals* '())
(define *global-syms* '())
(define (global-lambda? global) (cadr global))
(define (global-not-lambda? global) (not (cadr global)))
(define (add-global var-sym lambda? code)
  ;(write `(add-global ,var-sym ,code))
  (set! *globals* (cons (list var-sym lambda? code) *globals*)))
(define (c-compile-global exp append-preamble cont)
 (let ((var (define->var exp))
       (body (if (equal? 4 (length exp)) ; Simple var assignment contains superfluous %closure-ref
                 (cadddr exp)
                 (car (define->exp exp)))))
   (add-global 
     var 
     (lambda? body) 
     (c-compile-exp body append-preamble cont))
   (c-code/vars "" (list ""))))

;; Symbol compilation

(define *symbols* '())

; These are (at least for now) preallocated by the runtime
(define *reserved-symbols* '(Cyc_procedure))

(define (allocate-symbol sym)
  (if (and (not (member sym *symbols*))
           (not (member sym *reserved-symbols*)))
      (set! *symbols* (cons sym *symbols*))))

;; Lambda compilation.

;; Lambdas get compiled into procedures that, 
;; once given a C name, produce a C function
;; definition with that name.

;; These procedures are stored up an eventually 
;; emitted.

; type lambda-id = natural

; num-lambdas : natural
(define num-lambdas 0)

; lambdas : alist[lambda-id,string -> string]
(define lambdas '())

; allocate-lambda : (string -> string) -> lambda-id
(define (allocate-lambda lam)
  (let ((id num-lambdas))
    (set! num-lambdas (+ 1 num-lambdas))
    (set! lambdas (cons (list id lam) lambdas))
    id))

; get-lambda : lambda-id -> (symbol -> string)
(define (get-lambda id)
  (cdr (assv id lambdas)))

(define (lambda->env exp)
    (let ((formals (lambda-formals->list exp)))
        (if (pair? formals)
            (car formals)
            'unused)))

;; c-compile-closure : closure-exp (string -> void) -> string
;;
;; This function compiles closures generated earlier in the
;; compilation process.  Each closure is of the form:
;;
;;   (%closure lambda arg ...)
;;
;; Where:
;;  - `%closure` is the identifying tag
;;  - `lambda` is the function to execute
;;  - Each `arg` is a free variable that must be stored within
;;    the closure. The closure conversion phase tags each access
;;    to one with the corresponding index so `lambda` can use them.
;;
(define (c-compile-closure exp append-preamble cont)
  (let* ((lam (closure->lam exp))
         (free-vars
           (map
             (lambda (free-var)
                (if (tagged-list? '%closure-ref free-var)
                    (let ((var (cadr free-var))
                          (idx (number->string (- (caddr free-var) 1))))
                        (string-append 
                            "((closureN)" (mangle var) ")->elts[" idx "]"))
                    (mangle free-var)))
             (closure->fv exp))) ; Note these are not necessarily symbols, but in cc form
         (cv-name (mangle (gensym 'c)))
         (lid (allocate-lambda (c-compile-lambda lam)))
         (create-nclosure (lambda ()
           (string-append
             "closureN_type " cv-name ";\n"
             cv-name ".tag = closureN_tag;\n "
             cv-name ".fn = __lambda_" (number->string lid) ";\n"
             cv-name ".num_elt = " (number->string (length free-vars)) ";\n"
             cv-name ".elts = (object *)alloca(sizeof(object) * " 
                     (number->string (length free-vars)) ");\n"
             (let loop ((i 0) 
                        (vars free-vars))
               (if  (null? vars)
                 ""
                 (string-append 
                   cv-name ".elts[" (number->string i) "] = " 
                           (car vars) ";\n"
                   (loop (+ i 1) (cdr vars))))))))
         (create-mclosure (lambda () 
           (string-append
            "mclosure" (number->string (length free-vars)) "(" cv-name ", "
            ;; NOTE:
            ;; Hopefully will not cause issues with varargs when casting to
            ;; generic function type below. Works fine in gcc, not sure if 
            ;; this is portable to other compilers though
            "(function_type)__lambda_" (number->string lid)
            (if (> (length free-vars) 0) "," "")
            (string-join free-vars ", ")
            ");"))))
  (c-code/vars
    (string-append "&" cv-name)
    (list 
      (if (> (length free-vars) 0)
        (create-nclosure)
        (create-mclosure))))))

; c-compile-formals : list[symbol] -> string
(define (c-compile-formals formals type)
  (if (not (pair? formals))
      ""
      (string-append
       "object "
       (mangle (car formals))
       (cond
         ((pair? (cdr formals))
          (string-append ", " (c-compile-formals (cdr formals) type)))
         ((not (equal? 'args:fixed type)) 
          (string-append ", object " (mangle (cdr formals)) "_raw, ..."))
         (else
          "")))))

; c-compile-lambda : lamda-exp (string -> void) -> (string -> string)
(define (c-compile-lambda exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n")))))
    (let* ((formals (c-compile-formals 
                      (lambda->formals exp)
                      (lambda-formals-type exp)))
           (tmp-ident (if (> (length (lambda-formals->list exp)) 0) 
                          (mangle (car (lambda->formals exp)))
                          ""))
           (has-closure? 
             (and
               (> (string-length tmp-ident) 3)
               (equal? "self" (substring tmp-ident 0 4))))
           (formals*
             (string-append
                (if has-closure? 
                    "" 
                    (if (equal? "" formals) 
                        "closure _"    ;; TODO: seems wrong, will GC be too aggressive 
                        "closure _,")) ;; due to missing refs, with ignored closure?
                formals))
           (env-closure (lambda->env exp))
           (body    (c-compile-exp     
                        (car (lambda->exp exp)) ;; car ==> assume single expr in lambda body after CPS
                        append-preamble
                        (mangle env-closure))))
     (cons 
      (lambda (name)
        (string-append "static void " name 
                       "(int argc, " 
                        formals*
                       ") {\n"
                       preamble
                       (if (lambda-varargs? exp)
                         ;; Load varargs from C stack into Scheme list
                         (string-append 
                           ; DEBUGGING:
                           ;"printf(\"%d %d\\n\", argc, " 
                           ;  (number->string (length (lambda-formals->list exp))) ");"
                           "load_varargs(" 
                           (mangle (lambda-varargs-var exp))
                           ", "
                           (mangle (lambda-varargs-var exp))
                           "_raw, argc - " (number->string 
                                         (- (length (lambda-formals->list exp)) 
                                            1
                                            (if has-closure? 1 0)))
                           ");\n");
                         "") ; No varargs, skip
                       (c:serialize body "  ") "; \n"
                       "}\n"))
      formals*))))
  
(define (mta:code-gen input-program globals program? lib-exports lib-imports)
  (set! *global-syms* globals)
  (let ((compiled-program 
          (apply string-append
            (map c-compile-program input-program))))
    (if (member 'eval globals)
      (emit "#define CYC_EVAL"))

    (emit-c-arity-macros 0)
    (emit "#include \"cyclone.h\"")
    (c-macro-declare-globals)
    (emit "#include \"runtime.h\"")
    (emit "#include \"runtime-main.h\"")

    ;; Emit symbols
    (for-each
        (lambda (sym)
            (emit 
              (string-append 
                "defsymbol(" (mangle sym) ", " (symbol->string sym) ");")))
        *symbols*)

    ;; Emit lambdas:
    ; Print the prototypes:
    (for-each
     (lambda (l)
       (emit (string-append 
               "static void __lambda_" 
               (number->string (car l)) "(int argc, "
               (cdadr l)
               ") ;")))
     lambdas)
    
    (emit "")
    
    ; Print the definitions:
    (for-each
     (lambda (l)
       (emit ((caadr l) (string-append "__lambda_" (number->string (car l))))))
     lambdas)
  
    (emit "
  static void c_entry_pt(argc, env,cont) int argc; closure env,cont; { ")

    ;; Initialize global table
    (c-macro-GC-globals)

    ;; Initialize symbol table
    (for-each
        (lambda (sym)
            (emit (string-append "  add_symbol(quote_" (mangle sym) ");")))
        *symbols*)

    ;; Initialize globals
    (let* ((prefix "  ")
           (emit-global
             (lambda (global)
               (emits (c:allocs->str2 (c:allocs (caddr global)) prefix " \n"))
               (emits prefix)
               (emits (mangle-global (car global)))
               (emits " = ")
               (emits (c:body (caddr global)))
               (emit "; "))))
      (for-each emit-global (filter global-lambda? *globals*))
      (for-each emit-global (filter global-not-lambda? *globals*))
      (emit ""))

    ;; Initialize Cyc_global_variables
    ;; TODO: only need to do this if 'eval' was also compiled
    (let ((pairs '())
          (head-pair #f))
        (for-each
          (lambda (g)
            (let ((cvar-sym (mangle (gensym 'cvar)))
                  (pair-sym (mangle (gensym 'pair))))
             (emits 
               (string-append 
                 "  make_cvar(" cvar-sym 
                 ", (object *)&" (mangle-global (car g)) ");"))
             (emits
               (string-append
                 "make_cons(" pair-sym ", find_or_add_symbol(\"" (symbol->string (car g))
                 "\"), &" cvar-sym ");\n"))
             (set! pairs (cons pair-sym pairs))
          ))
          *globals*)
        (let loop ((code '())
                   (ps pairs)
                   (cs (map (lambda (_) (mangle (gensym 'c))) pairs)))
            (cond
              ((null? ps) 
               (for-each
                 (lambda (str)
                   (emits str))
                 code))
              ((null? (cdr ps))
               (loop (cons (string-append "make_cons(" (car cs) ", &" (car ps) ",nil);\n") code)
                     (cdr ps)
                     (cdr cs)))
              (else
               (if (not head-pair)
                   (set! head-pair (car cs)))
               (loop (cons (string-append "make_cons(" (car cs) ", &" (car ps) ", &" (cadr cs) ");\n") code)
                     (cdr ps) 
                     (cdr cs)))))
        (if head-pair
            (emits
              (string-append "Cyc_global_variables = &" head-pair ";"))))

    (emit compiled-program)
    (emit "}")
    (emit *c-main-function*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically generate blocks of code for the compiler
(define (autogen filename)
  (let ((fp (open-output-file filename)))
    (autogen:defprimitives fp)
    (autogen:primitive-procedures fp)
    (close-output-port fp)))

(define (autogen:defprimitives fp)
  (display "/* This section is auto-generated via --autogen */\n" fp)
  (for-each
    (lambda (p)
      (display
        (string-append 
          "defprimitive(" 
          (mangle p)
          ", "
          (symbol->string p)
          ", &_"
          (mangle p)
          "); /* "
          (symbol->string p)
          " */\n")
        fp))
    *primitives*)
  (display "/* -------------------------------------------- */\n" fp))

;; List of primitive procedures
(define (autogen:primitive-procedures fp)
  (let ((code 
          (cons 
            'list
            (map
              (lambda (p)
                `(list (quote ,p) ,p))
               *primitives*))))
    (cond-expand
      (chicken
       (pp code fp)) ;; CHICKEN pretty-print
      (else
       (write code fp)))))


