;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module compiles scheme code to a Cheney-on-the-MTA C runtime.
;;;;
(define-library (scheme cyclone cgen)
  (import (scheme base)
          (scheme char)
          (scheme complex)
          (scheme eval)
          (scheme inexact)
          (scheme write)
          (scheme cyclone primitives)
          (scheme cyclone transforms)
          (scheme cyclone ast)
          (scheme cyclone cps-optimizations)
          (scheme cyclone util)
          (scheme cyclone libraries)
  )
  (export
    mta:code-gen
    autogen
    autogen:defprimitives 
    autogen:primitive-procedures 
    ;c-compile-program
    emit
    emit*
    emits
    emits*
    emit-newline
  )
  (inline
    global-not-lambda?
    global-lambda?
    c:num-args
    c:allocs
    st:->var
  )
  (begin

(define (emit line)
  (display line)
  (newline))

(define (emit* . strs)
  (for-each emits strs)
  (newline))

(define (emits str)
  (display str))

(define (emits* . strs)
  (for-each emits strs))

(define (emit-newline)
  (newline))

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
                     ((equal? (car head) #\alarm)
                      (next (cdr head) (cons #\a (cons #\\ tail))))
                     ((equal? (car head) #\backspace)
                      (next (cdr head) (cons #\b (cons #\\ tail))))
                     ((equal? (car head) #\return)
                      (next (cdr head) (cons #\r (cons #\\ tail))))
                     ((equal? (car head) #\tab)
                      (next (cdr head) (cons #\t (cons #\\ tail))))
                     (else
                       (next (cdr head) (cons (car head) tail)))))))
    (next (string->list str) '())))

(define *c-main-function*
"int main(int argc, char **argv, char **envp)
{gc_thread_data *thd;
 long stack_size = global_stack_size = STACK_SIZE;
 long heap_size = global_heap_size = HEAP_SIZE;
 mclosure0(clos_halt,&Cyc_halt);  // Halt if final closure is reached
 mclosure0(entry_pt,&c_entry_pt); // First function to execute
 _cyc_argc = argc;
 _cyc_argv = argv;
 set_env_variables(envp);
 gc_initialize();
 thd = malloc(sizeof(gc_thread_data));
 gc_thread_data_init(thd, 0, (char *) &stack_size, stack_size);
 thd->gc_cont = &entry_pt;
 thd->gc_args[0] = &clos_halt;
 thd->gc_num_args = 1;
 thd->thread_id = pthread_self();
 gc_add_mutator(thd);
 Cyc_heap_init(heap_size);
 thd->thread_state = CYC_THREAD_STATE_RUNNABLE;
 Cyc_start_trampoline(thd);
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
       (emit (c-macro-closcall arity))
       (emit (c-macro-return-closcall arity))
       (emit (c-macro-return-direct arity))))
    (emit-c-arity-macros (+ arity 1))))

(define (c-macro-return-closcall num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
      ;"/* Check for GC, then call given continuation closure */\n"
      "#define return_closcall" n "(td, clo" args ") { \\\n"
      " char top; \\\n"
      " if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \\\n"
      "     object buf[" n "]; " arry-assign "\\\n"
      "     GC(td, clo, buf, " n "); \\\n"
      "     return; \\\n"
      " } else {\\\n"
      "     closcall" n "(td, (closure) (clo)" args "); \\\n"
      "     return;\\\n"
      " } \\\n"
      "}\n")))

(define (c-macro-return-direct num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
      ;"/* Check for GC, then call C function directly */\n"
      "#define return_direct" n "(td, _fn" args ") { \\\n"
      " char top; \\\n"
      " if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \\\n"
      "     object buf[" n "]; " arry-assign " \\\n"
      "     mclosure0(c1, (function_type) _fn); \\\n"
      "     GC(td, &c1, buf, " n "); \\\n"
      "     return; \\\n"
      " } else { \\\n"
      "     (_fn)(td, " n ", (closure)_fn" args "); \\\n"
      " }}\n")))

(define (c-macro-closcall num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (n-1 (number->string (if (> num-args 0) (- num-args 1) 0)))
        (wrap (lambda (s) (if (> num-args 0) s ""))))
    (string-append
      "#define closcall" n "(td, clo" args ") \\\n"
        (wrap (string-append "if (type_is_pair_prim(clo)) { \\\n"
                             "   Cyc_apply(td, " n-1 ", (closure)(a1), clo" (if (> num-args 1) (substring args 3 (string-length args)) "") "); \\\n"
                             "}"))
        (wrap " else { \\\n")
        "   ((clo)->fn)(td, " n ", clo" args ")"
        (wrap ";\\\n}")
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

;;; Stack trace (call history) helpers

;; Add function to trace, if not already set
(define (st:add-function! trace fnc)
  (if (null? (cdr trace))
    (cons (car trace) fnc)
    trace))

(define (st:->code trace)
  (if (or (not (pair? trace))
          (null? (cdr trace)))
    ""
    (string-append 
      "Cyc_st_add(data, \""
      (car trace) 
      ":" 
      ;; TODO: escape backslashes
      (symbol->string (cdr trace))
      "\");\n"
      )))

(define (st:->var trace)
  (cdr trace))
;; END st helpers

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
  (foldr
    (lambda (x y)
      (string-append
        (string-append
            (if (null? prefix)
                ""
                (car prefix))
            x 
            "\n")
        y))
    ""
    c-allocs))

(define (c:allocs->str2 c-allocs prefix suffix)
  (foldr
    (lambda (x y)
      (string-append
        (string-append prefix x suffix)))
    ""
    c-allocs))

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
(define (c-compile-program exp src-file)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble "cont" (list src-file) #t)))
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
;; trace - trace information. presently a pair containing:
;;         * source file
;;         * function name (or NULL if none)
;; cps? - Determine whether to compile using continuation passing style.
;;        Normally this is always enabled, but sometimes a function has a
;;        version that can be inlined (as an optimization), so this will 
;;        be set to false to change the type of compilation.
;;        NOTE: this field is not passed everywhere because a lot of forms
;;              require CPS, so this flag is not applicable to them.
(define (c-compile-exp exp append-preamble cont trace cps?)
  (cond
    ; Core forms:
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       
     ;; TODO: this needs to be more refined, probably w/a lookup table
     (c-code (string-append "primitive_" (mangle exp))))
    ((ref?   exp)       (c-compile-ref exp))
    ((quote? exp)       (c-compile-quote exp))
    ((if? exp)          (c-compile-if exp append-preamble cont trace cps?))

    ; IR (2):
    ((tagged-list? '%closure exp)
     (c-compile-closure exp append-preamble cont trace cps?))
    ; Global definition
    ((define? exp)
     (c-compile-global exp append-preamble cont trace))
    ((define-c? exp)
     (c-compile-raw-global-lambda exp append-preamble cont trace))
    ; Special case - global function w/out a closure. Create an empty closure
    ((tagged-list? 'lambda exp)
     (c-compile-exp
      `(%closure ,exp)
       append-preamble cont trace cps?))
    
    ; Application:      
    ((app? exp)         (c-compile-app exp append-preamble cont trace cps?))
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
          (string-append "make_pair(" cvar "," (c:body a) "," (c:body b) ");")
          (append (c:allocs a) (c:allocs b))))
    )
    (_c-compile-scalars 
     (lambda (args)
       (cond
        ((null? args)
           (c-code "NULL"))
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

(define (c-compile-vector exp)
  (letrec ((cvar-name (mangle (gensym 'vec)))
           (len (vector-length exp))
           ;; Generate code for each member of the vector 
           (loop 
            (lambda (i code)
              (if (= i len)
                code
                (let ((idx-code (c-compile-const (vector-ref exp i))))
                  (loop 
                    (+ i 1)
                    (c-code/vars
                      ;; The vector's C variable
                      (c:body code)
                      ;; Allocations
                      (append
                        (c:allocs code) ;; Vector alloc
                        (c:allocs idx-code) ;; Member alloc at index i
                        (list ;; Assign this member to vector
                          (string-append 
                            cvar-name ".elements[" (number->string i) "] = "
                            (c:body idx-code)
                            ";")))))))))
          )
    (cond
      ((zero? len)
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list ; Allocate empty vector
              (string-append 
                "make_empty_vector(" cvar-name ");"))))
      (else
        (let ((code
                (c-code/vars
                  (string-append "&" cvar-name) ; Code body is just var name
                  (list ; Allocate the vector
                    (string-append 
                      "make_empty_vector(" cvar-name ");"
                      cvar-name ".num_elements = " (number->string len) ";"
                      cvar-name ".elements = (object *)alloca(sizeof(object) * " 
                                         (number->string len) ");")))))
        (loop 0 code))))))

(define (c-compile-bytevector exp)
  (letrec ((cvar-name (mangle (gensym 'vec)))
           (len (bytevector-length exp))
           ;; Generate code for each member of the vector 
           (loop 
            (lambda (i code)
              (if (= i len)
                code
                (let ((byte-val (number->string (bytevector-u8-ref exp i))))
                  (loop 
                    (+ i 1)
                    (c-code/vars
                      ;; The bytevector's C variable
                      (c:body code)
                      ;; Allocations
                      (append
                        (c:allocs code) ;; Vector alloc
                        (list ;; Assign this member to vector
                          (string-append 
                            cvar-name ".data[" (number->string i) "] = (unsigned char)"
                            byte-val
                            ";"))))
                    ))))
            )
          )
    (cond
      ((zero? len)
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list ; Allocate empty vector
              (string-append 
                "make_empty_bytevector(" cvar-name ");"))))
      (else
        (let ((code
                (c-code/vars
                  (string-append "&" cvar-name) ; Code body is just var name
                  (list ; Allocate the vector
                    (string-append 
                      "make_empty_bytevector(" cvar-name ");"
                      cvar-name ".len = " (number->string len) ";"
                      cvar-name ".data = alloca(sizeof(char) * " 
                                         (number->string len) ");")))))
        (loop 0 code))))))

;; c-compile-const : const-exp -> c-pair
;;
;; Typically this function is used to compile constant values such as
;; a single number, boolean, etc. However, it can be passed a quoted
;; item such as a list, to compile as a literal.
(define (c-compile-const exp)
  (cond
    ((null? exp)
     (c-code "NULL"))
    ((pair? exp)
     (c-compile-scalars exp))
    ((vector? exp)
     (c-compile-vector exp))
    ((bytevector? exp)
     (c-compile-bytevector exp))
    ((bignum? exp)
      (let ((cvar-name (mangle (gensym 'c)))
            (num2str (cond
                       (else
                         (number->string exp)))))
        (c-code/vars
            (string-append "" cvar-name) ; Code is just the variable name
            (list     ; Allocate pointer on the C stack
              (string-append 
                "alloc_bignum(data, " cvar-name "); "
                ;; TODO: need error checking, this is just a first cut:
                "mp_read_radix(&bignum_value(" cvar-name "), \"" num2str "\", 10);"))))
    )
    ((complex? exp)
      (let* ((cvar-name (mangle (gensym 'c)))
             (num2str (lambda (n)
                        (cond
                          ;; The following two may not be very portable, 
                          ;; may be better to use C99:
                          ((nan? n) "(0./0.)")
                          ((infinite? n) "(1./0.)")
                          (else
                            (number->string n)))))
             (rnum (num2str (real-part exp)))
             (inum (num2str (imag-part exp)))
            )
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list     ; Allocate on the C stack
              (string-append 
                "make_complex_num(" cvar-name ", " rnum ", " inum ");")))))
    ((integer? exp) 
;     (let ((cvar-name (mangle (gensym 'c))))
;        (c-code/vars
;            (string-append "&" cvar-name) ; Code is just the variable name
;            (list     ; Allocate integer on the C stack
;              (string-append 
;                "make_int(" cvar-name ", " (number->string exp) ");")))))
     (c-code (string-append "obj_int2obj(" 
               (number->string exp) ")")))
    ((real? exp)
      (let ((cvar-name (mangle (gensym 'c)))
            (num2str (cond
                       ;; The following two may not be very portable, 
                       ;; may be better to use C99:
                       ((nan? exp) "(0./0.)")
                       ((infinite? exp) "(1./0.)")
                       (else
                         (number->string exp)))))
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list     ; Allocate on the C stack
              (string-append 
                "make_double(" cvar-name ", " num2str ");")))))
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
                "make_utf8_string_with_len(" 
                cvar-name 
                ", " 
                (->cstr exp) 
                ", " 
                (number->string (string-byte-length exp))
                ", " 
                (number->string (string-length exp))
                ");")))))
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

(define-c string-byte-length
  "(void *data, int argc, closure _, object k, object s)"
  " return_closcall1(data, k, Cyc_string_byte_length(data, s)); ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives

;; Does string end with the given substring?
;; EG: ("test(" "(") ==> #t
(define (str-ending? str end)
  (let ((len (string-length str)))
    (and (> len 0)
         (equal? end (substring str (- len 1) len)))))

;; c-compile-prim : prim-exp -> string -> string
(define (c-compile-prim p cont)
  (let* ((c-func 
           (if (prim:udf? p)
               (string-append
                 "((inline_function_type)
                   ((closure)"
                    (cgen:mangle-global p)
                 ")->fn)")
               (prim->c-func p)))
         ;; Following closure defs are only used for prim:cont? to
         ;; create a new closure for the continuation, if needed.
         ;;
         ;; Each prim:cont? function is different in that it takes a continuation so that it can
         ;; allocate arbitrary data as needed using alloca, and then call into
         ;; the cont so allocations can remain on stack until GC.
         (closure-sym (mangle (gensym 'c)))
         (closure-def
           (cond
             ((and (prim:cont? p)
                   (> (string-length cont) (string-length "__lambda_"))
                   (equal? (substring cont 0 9) "__lambda_"))
              (string-append 
                "mclosure0(" closure-sym 
                "," cont "); "))
             (else #f)))
         ;; END apply defs
         (tdata (cond
                 ((prim/data-arg? p) "data")
                 (else "")))
         (tdata-comma (if (> (string-length tdata) 0) "," ""))
         (tptr-type (prim/c-var-pointer p))
         (tptr-comma (if tptr-type ",&" ""))
         (tptr (cond
                (tptr-type (mangle (gensym 'local)))
                (else "")))
         (tptr-decl
          (cond 
            (tptr-type (string-append tptr-type " " tptr "; "))
            (else "")))
         (c-var-assign 
            (lambda (type)
              (let ((cv-name (mangle (gensym 'c))))
                (c-code/vars 
                  (string-append 
                    (if (or (prim:cont? p) 
                            (equal? (prim/c-var-assign p) "object")
                            (prim/c-var-pointer p)) ;; Assume returns object
                        "" 
                        "&")
                    cv-name)
                  (list
                    (string-append 
                      ;; Define closure if necessary (apply only)
                      (cond
                       (closure-def closure-def)
                       (else ""))

                      ;; Emit C variables
                      tptr-decl
                      type " " cv-name " = " c-func "("

                      ;; Emit closure as first arg, if necessary (apply only)
                      (cond
                       (closure-def
                        (string-append 
                          tdata
                          tptr-comma tptr
                          ",&" closure-sym))
                       ((prim:cont? p) 
                        (string-append 
                          tdata 
                          tptr-comma tptr
                          ","
                          cont))
                       (else 
                        (string-append
                          tdata tptr-comma tptr))))))))))
    (cond
     ((prim/c-var-assign p)
      (c-var-assign (prim/c-var-assign p)))
     ((prim/cvar? p)
        ;;
        ;; TODO: look at functions that would actually fall into this
        ;; branch, I think they are just the macro's like list->vector???
        ;; may be able to remove this using prim:cont? and simplify
        ;; the logic
        ;;
        (let ((cv-name (mangle (gensym 'c))))
           (c-code/vars
            (if (prim:allocates-object? p)
                cv-name ;; Already a pointer
                (string-append "&" cv-name)) ;; Point to data
            (list
                (string-append c-func "(" cv-name tdata-comma tdata)))))
     (else
        (c-code (string-append c-func "(" tdata))))))

;; END primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; c-compile-ref : ref-exp -> string
(define (c-compile-ref exp)
  (c-code 
    (if (member exp *global-syms*)
      (cgen:mangle-global exp)
      (mangle exp))))

; c-compile-args : list[exp] (string -> void) -> string
(define (c-compile-args args append-preamble prefix cont trace cps?)
  (letrec ((num-args 0)
         (_c-compile-args 
          (lambda (args append-preamble prefix cont)
            (cond
             ((not (pair? args))
              (c-code ""))
             (else
              ;(trace:debug `(c-compile-args ,(car args)))
              (set! num-args (+ 1 num-args))
              (c:append/prefix
                prefix 
                (c-compile-exp (car args) 
                  append-preamble cont trace cps?)
                (_c-compile-args (cdr args) 
                  append-preamble ", " cont)))))))
  (c:tuple/args
    (_c-compile-args args 
      append-preamble prefix cont)
    num-args)))

;; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble cont trace cps?)
  ;;(trace:info `(c-compile-app: ,exp ,trace))
  (let (($tmp (mangle (gensym 'tmp))))
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
      (cond
        ;; Direct recursive call of top-level function
        ((and (pair? trace)
              (not (null? (cdr trace)))
              (adbv:direct-rec-call? (adb:get (cdr trace)))
              (tagged-list? '%closure-ref fun)
              (equal? (cadr fun) (cdr trace)) ;; Needed?
              (equal? (car args) (cdr trace))
              ;; Make sure continuation is not a lambda, because
              ;; that means a closure may be allocated
              (ref? (cadr args))
         )
         (let* ((cgen-lis
                  (map 
                    (lambda (e)
                     (c-compile-exp e append-preamble "" "" cps?))
                    (cddr args)) ;; Skip the closure
                  )
                (cgen-allocs 
                  (apply string-append 
                    (map (lambda (a) (c:allocs->str (c:allocs a))) cgen-lis)))

                (parent-fnc (adbv:assigned-value (adb:get (cdr trace))))
                (parent-args 
                  (cdr ;; Skip continuation
                    (ast:lambda-args 
                      (if (pair? parent-fnc) 
                          (car parent-fnc)
                          parent-fnc))))
                (cgen-body
                  (apply
                    string-append
                    (map
                      (lambda (arg body-exp)
                        (string-append
                          (mangle arg)
                          " = "
                          (c:body body-exp)
                          ";"
                        )
                      )
                      parent-args
                      cgen-lis)))
               )
           ;;(trace:info `(loop ,args ,(cadr args) ,cgen-lis ,parent-args))
           (c-code
             (string-append
               cgen-allocs ;(c:allocs->str (c:allocs cgen))
               "\n"
               cgen-body ;; TODO: (c:body cgen) ;; TODO: re-assign function args, longer-term using temp variables
               "\n"
               "goto loop;")))
        )
         
        ((lambda? fun)
         (let* ((lid (allocate-lambda (c-compile-lambda fun trace #t))) ;; TODO: pass in free vars? may be needed to track closures
                                                               ;; properly, wait until this comes up in an example
                (this-cont (string-append "__lambda_" (number->string lid)))
                (cgen 
                  (c-compile-args
                     args 
                     append-preamble 
                     ""
                     this-cont
                     trace
                     cps?))
                (num-cargs (c:num-args cgen)))
           (set-c-call-arity! num-cargs)
           (c-code
             (string-append
               (c:allocs->str (c:allocs cgen))
               "return_direct" (number->string num-cargs) 
               "(data," this-cont
               (if (> num-cargs 0) "," "") ; TODO: how to propagate continuation - cont " "
                  (c:body cgen) ");"))))

        ((prim? fun)
         (let* ((c-fun 
                 (c-compile-prim fun cont))
                (c-args
                 (c-compile-args args append-preamble "" "" trace cps?))
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
                    (if (prim/c-var-assign fun)
                      ;; Add a comma if there were any args to the func added by comp-prim
                      (if (or (str-ending? (car (c:allocs c-fun)) "(") 
                              (prim:cont/no-args? fun)
                              (and (prim:udf? fun)
                                   (zero? num-args)))
                        "" 
                        ",")
                      ",")
                    (c:body c-args*) ");"))))
              ;; Args stay with body
              (c:append
                (c:append 
                  (let ()
                    ;; Add a comma if necessary
                    (if (or (str-ending? (c:body c-fun) "(")
                            (prim:cont/no-args? fun))
                      c-fun
                      (c:append c-fun (c-code ", "))))
                  c-args*)
                (c-code ")")))))

        ((equal? '%closure-ref fun)
         (c-code (apply string-append (list
            "("
            ;; TODO: probably not the ideal solution, but works for now
            "(closureN)"
            (mangle (car args))
            ")->elements["
            (number->string (- (cadr args) 1))"]"))))

        ;; TODO: may not be good enough, closure app could be from an element
        ((tagged-list? '%closure-ref fun)
         (let* ((cfun (c-compile-args (list (car args)) append-preamble "  " cont trace cps?))
                (this-cont (c:body cfun))
                (cargs (c-compile-args (cdr args) append-preamble "  " this-cont trace cps?)))
           (cond
             ((not cps?)
              (c-code 
                (string-append
                  (c:allocs->str (c:allocs cfun) "\n")
                  (c:allocs->str (c:allocs cargs) "\n")
                  "return_copy(ptr,"
                  (c:body cargs)
                  ");")))
             (else
              (set-c-call-arity! (c:num-args cargs))
              (c-code 
                (string-append
                  (c:allocs->str (c:allocs cfun) "\n")
                  (c:allocs->str (c:allocs cargs) "\n")
                  "return_closcall" (number->string (c:num-args cargs))
                  "(data,"
                  this-cont
                  (if (> (c:num-args cargs) 0) "," "")
                  (c:body cargs)
                  ");"))))))

        ((tagged-list? '%closure fun)
         (let* ((cfun (c-compile-closure 
                        fun append-preamble cont trace cps?))
                (this-cont (string-append "(closure)" (c:body cfun)))
                (cargs (c-compile-args
                         args append-preamble "  " this-cont trace cps?))
                (num-cargs (c:num-args cargs)))
           (cond
             ((not cps?)
              (c-code
                (string-append
                   (c:allocs->str (c:allocs cfun) "\n")
                   (c:allocs->str (c:allocs cargs) "\n")
                   "return_copy(ptr,"
                   (c:body cargs)
                   ");")))
             (else ;; CPS, IE normal behavior
               (set-c-call-arity! num-cargs)
               (c-code
                 (string-append
                    (c:allocs->str (c:allocs cfun) "\n")
                    (c:allocs->str (c:allocs cargs) "\n")
                    "return_closcall" (number->string num-cargs)
                    "(data,"
                    this-cont
                    (if (> num-cargs 0) "," "")
                    (c:body cargs)
                    ");"))))))

        ((equal? 'Cyc-seq fun)
         (let ((exps (foldr
                       (lambda (expr acc)
                         ;; Join expressions; based on c:append 
                         (let ((cp1 (if (ref? expr)
                                        ; Ignore lone ref to avoid C warning
                                        (c-code/vars "" '())
                                        (c-compile-exp expr append-preamble cont trace cps?)))
                               (cp2 acc))
                           (c-code/vars 
                             (let ((cp1-body (c:body cp1)))
                               (if (zero? (string-length cp1-body))
                                   (c:body cp2) ;; Ignore cp1 if necessary
                                   (string-append cp1-body ";" (c:body cp2))))
                             (append (c:allocs cp1) (c:allocs cp2)))))
                       (c-code "")
                       args)))
          exps))
        (else
         (error `(Unsupported function application ,exp)))))))

; c-compile-if : if-exp -> string
(define (c-compile-if exp append-preamble cont trace cps?)
  (let* ((compile (lambda (exp)
                    (c-compile-exp exp append-preamble cont trace cps?)))
         (test (compile (if->condition exp)))
         (then (compile (if->then exp)))
         (els (compile (if->else exp))))
  (c-code (string-append
   (c:allocs->str (c:allocs test) "  ")
   "if( (boolean_f != "
   (c:body test)
   ") ){ \n"
   (c:serialize then "  ")
   "\n} else { \n"
   (c:serialize els "  ")
   "}\n"))))

;; Global inlinable functions
(define *global-inlines* '())
(define (add-global-inline orig-sym inline-sym)
  (set! *global-inlines* (cons (cons orig-sym inline-sym) *global-inlines*)))

;; Add a global inlinable function that is written in Scheme.
;; This is more challenging than define-c forms since the 
;; code must be compiled again to work without CPS.
;(define *global-inline-scms* '())
;(define (add-global-inline-scm-lambda var-sym code)
;  (add-global-inline var-sym )
;  (set! *global-inline-scms* 
;        (cons (list var-sym code) *global-inline-scms*)))

;; Global compilation
(define *globals* '())
(define *global-syms* '())
(define (global-lambda? global) (cadr global))
(define (global-not-lambda? global) (not (cadr global)))
(define (add-global var-sym lambda? code)
  ;(write `(add-global ,var-sym ,code))
  (set! *globals* (cons (list var-sym lambda? code) *globals*)))
(define (c-compile-global exp append-preamble cont trace)
 (let ((var (define->var exp))
       (body (if (equal? 4 (length exp)) ; Simple var assignment contains superfluous %closure-ref
                 (cadddr exp)
                 (car (define->exp exp)))))
   (add-global 
     var 
     (lambda? body) 
     (c-compile-exp 
      body append-preamble cont
      (st:add-function! trace var) #t))

   ;; Add inline global definition also, if applicable
;   (trace:error `(JAE DEBUG ,var 
;           ,(lambda? body) 
;           ,(define-c->inline-var exp)
;           ,(prim:udf? (define-c->inline-var exp))
;           ))
   (when (and (lambda? body)
              (prim:udf? (define-c->inline-var exp)))
       (add-global-inline 
         var
         (define-c->inline-var exp))
       (add-global 
         (define-c->inline-var exp)
         #t ;; always a lambda
         (c-compile-exp 
          body append-preamble cont
          (st:add-function! trace var)
          #f ;; inline, so disable CPS on this pass
         )
        ))

   (c-code/vars "" (list ""))))

(define (c-compile-raw-global-lambda exp append-preamble cont trace . cps?)
   (let* ((precompiled-sym
            (if (equal? cps? '(#f))
                'precompiled-inline-lambda
                'precompiled-lambda))
          (lambda-data
            `(,precompiled-sym
              ,(caddr exp) ;; Args
              ,(cadddr exp) ;; Body
          ))
          (lid (allocate-lambda lambda-data))
          (total-num-args
            (let ((count 1)) ;; Start at 1 because there will be one less comma than args
              (string-for-each 
                (lambda (c) 
                  (if (equal? #\, c) (set! count (+ count 1))))
                (caddr exp))
              count)) ;; args
          ;; Subtract "internal" args added for runtime
          (num-args
            (- total-num-args 4))
          )
     ;; Is the function also defined inline?
     ;(trace:error `(JAE define-c ,exp))
     (cond
      ((> (length exp) 4)
       ;(trace:error `(JAE define-c inline detected))
       (let ((fnc-sym 
               (define-c->inline-var exp)))
          ;(trace:error `(JAE define-c inline detected ,fnc-sym))
         (add-global-inline (define->var exp) fnc-sym)
         (c-compile-raw-global-lambda 
           `(define-c ,fnc-sym ,@(cddddr exp))
           append-preamble 
           cont 
           trace
           #f)))) ;; Inline this one; CPS will not be used
     ;; Add this define-c
     (add-global 
       (define->var exp)
       #t ;(lambda? body) 
       (let ((cv-name (mangle (gensym 'c))))
         (c-code/vars 
           (string-append "&" cv-name)
           (list
             (string-append "mclosure0(" cv-name ", (function_type)__lambda_"
               (number->string lid) ");" cv-name ".num_args = "
               (number->string num-args)
               ";")))
       )
     )
     (c-code/vars "" (list ""))))

;; Symbol compilation

(define *symbols* '())

(define (allocate-symbol sym)
  (if (not (member sym *symbols*))
           ;(not (Cyc-reserved-symbol? sym)))
      (set! *symbols* (cons sym *symbols*))))

;; Lambda compilation.

;; Lambdas get compiled into procedures that, 
;; once given a C name, produce a C function
;; definition with that name.

;; These procedures are stored up and eventually 
;; emitted.

; type lambda-id = natural

; num-lambdas : natural
(define num-lambdas 0)

; lambdas : alist[lambda-id,string -> string]
(define lambdas '())
(define inline-lambdas '())

; allocate-lambda : (string -> string) -> lambda-id
(define (allocate-lambda lam . cps?)
  (let ((id num-lambdas))
    (set! num-lambdas (+ 1 num-lambdas))
    (set! lambdas (cons (list id lam) lambdas))
    (if (equal? cps? '(#f))
        (set! inline-lambdas (cons id inline-lambdas)))
    id))

; get-lambda : lambda-id -> (symbol -> string)
;(define (get-lambda id)
;  (cdr (assv id lambdas)))

(define (lambda->env exp)
    (let ((formals (lambda-formals->list exp)))
        (if (pair? formals)
            (car formals)
            'unused)))

;           (tmp-ident (if (> (length (lambda-formals->list exp)) 0) 
;                          (mangle (if (pair? (lambda->formals exp))
;                                      (car (lambda->formals exp))
;                                      (lambda->formals exp)))
;                          ""))
;           (has-closure? 
;             (and
;               (> (string-length tmp-ident) 3)
;               (equal? "self" (substring tmp-ident 0 4))))

;; Compute the minimum number of arguments a function expects.
;; Note this must be the count before additional closure/CPS arguments
;; are added, so we need to detect those and not include them.
(define (compute-num-args lam)
  (let ((count (lambda-num-args lam))) ;; Current arg count, may be too high
    (cond
      ((< count 0) -1) ;; Unlimited
      (else
        (let ((formals (lambda-formals->list lam)))
          (- count
             (if (fl/closure? formals) 1 0)
             (if (fl/cont? formals) 1 0)))))))

;; Formal list with a closure?
(define (fl/closure? lis)
  (cond
    ((null? lis) #f)
    (else
      (let ((arg (symbol->string (car lis))))
        (and
          (> (string-length arg) 4)
          (equal? "self$" (substring arg 0 5)))))))

;; Formal list with a continuation (k)?
(define (fl/cont? lis)
  (let ((check (lambda (lis)
                (cond
                  ((null? lis) #f)
                  (else
                    (let ((arg (symbol->string (car lis))))
                      (and
                        (> (string-length arg) 1)
                        (equal? "k$" (substring arg 0 2)))))))))
    ;; Find the cont arg; if there is a closure it is always first
    (if (fl/closure? lis)
      (check (cdr lis))
      (check lis))))

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
(define (c-compile-closure exp append-preamble cont trace cps?)
  (let* ((lam (closure->lam exp))
         (free-vars
           (map
             (lambda (free-var)
                (if (tagged-list? '%closure-ref free-var)
                    (let ((var (cadr free-var))
                          (idx (number->string (- (caddr free-var) 1))))
                        (string-append 
                            "((closureN)" (mangle var) ")->elements[" idx "]"))
                    (mangle free-var)))
             (closure->fv exp))) ; Note these are not necessarily symbols, but in cc form
         (cv-name (mangle (gensym 'c)))
         (lid (allocate-lambda (c-compile-lambda lam trace cps?) cps?))
         (macro? (assoc (st:->var trace) (get-macros)))
         (call/cc? (and (equal? (car trace) "scheme/base.sld")
                        (equal? (st:->var trace) 'call/cc)))
         (num-args-str 
          (if call/cc?
            "1" ;; Special case, need to change runtime checks for call/cc
            (number->string (compute-num-args lam))))
         (create-nclosure (lambda ()
           (string-append
             "closureN_type " cv-name ";\n"
             ;; Not ideal, but one more special case to type check call/cc
             (if call/cc?  "Cyc_check_proc(data, f);\n" "")
             cv-name ".hdr.mark = gc_color_red;\n "
             cv-name ".hdr.grayed = 0;\n"
             cv-name ".tag = closureN_tag;\n "
             cv-name ".fn = (function_type)__lambda_" (number->string lid) ";\n"
             cv-name ".num_args = " num-args-str ";\n"
             cv-name ".num_elements = " (number->string (length free-vars)) ";\n"
             cv-name ".elements = (object *)alloca(sizeof(object) * " 
                     (number->string (length free-vars)) ");\n"
             (let loop ((i 0) 
                        (vars free-vars))
               (if  (null? vars)
                 ""
                 (string-append 
                   cv-name ".elements[" (number->string i) "] = " 
                           (car vars) ";\n"
                   (loop (+ i 1) (cdr vars))))))))
         (create-mclosure (lambda () 
           (let ((prefix 
                    (if macro?
                      "mmacro"
                      (string-append 
                        "mclosure" 
                        (number->string (length free-vars))))))
             (string-append
              prefix
              "(" cv-name ", "
              ;; NOTE:
              ;; Hopefully will not cause issues with varargs when casting to
              ;; generic function type below. Works fine in gcc, not sure if 
              ;; this is portable to other compilers though
              "(function_type)__lambda_" (number->string lid)
              (if (> (length free-vars) 0) "," "")
              (string-join free-vars ", ")
              ");"
              cv-name ".num_args = " (number->string (compute-num-args lam)) ";"
              )))))
  ;(trace:info (list 'JAE-DEBUG trace macro?))
  (c-code/vars
    (string-append "&" cv-name)
    (list 
      (if (> (length free-vars) 0)
        (create-nclosure)
        (create-mclosure))))))

; c-compile-formals : list[symbol] -> string
(define (c-compile-formals formals type)
  (cond
   ((and (not (pair? formals))
         (equal? type 'args:varargs))
    (string-append "object " (mangle formals) "_raw, ..."))
   ((not (pair? formals))
    "")
   (else
    (string-append
     "object "
     (mangle (car formals))
     (cond
       ((pair? (cdr formals))
        (string-append ", " (c-compile-formals (cdr formals) type)))
       ((not (equal? 'args:fixed type)) 
        (string-append ", object " (mangle (cdr formals)) "_raw, ..."))
       (else
        ""))))))

; c-compile-lambda : lamda-exp (string -> void) -> (string -> string)
(define (c-compile-lambda exp trace cps?)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n")))))
    (let* ((formals (c-compile-formals 
                      (if (not cps?)
                          ;; Ignore continuation (k) arg for non-CPS funcs
                          (cdr (lambda->formals exp))
                          (lambda->formals exp))
                      (lambda-formals-type exp)))
           (tmp-ident (if (> (length (lambda-formals->list exp)) 0) 
                          (mangle (if (pair? (lambda->formals exp))
                                      (car (lambda->formals exp))
                                      (lambda->formals exp)))
                          ""))
           (return-type
            (if cps? "void" "object"))
           (arg-argc (if cps? "int argc, " ""))
           (arg-closure
            (if cps?
                "closure _"
                "object ptr"))
           (has-closure? 
             (and
               (> (string-length tmp-ident) 3)
               (equal? "self" (substring tmp-ident 0 4))))
           (has-loop?
             (and (not has-closure?) ;; Only top-level functions for now
                  (pair? trace)
                  (not (null? (cdr trace)))
                  (adbv:direct-rec-call? (adb:get (cdr trace)))))
           (formals*
             (string-append
                (if has-closure? 
                    "" 
                    (if (equal? "" formals) 
                        arg-closure
                        (string-append arg-closure ",")))
                formals))
           (env-closure (lambda->env exp))
           (body    (c-compile-exp     
                        (car (lambda->exp exp)) ;; car ==> assume single expr in lambda body after CPS
                        append-preamble
                        (mangle env-closure)
                        trace 
                        cps?)))
     (cons 
      (lambda (name)
        (string-append "static " return-type " " name 
                       "(void *data, " arg-argc
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
                       (c:serialize
                         (c:append
                           (c-code 
                             ;; Only trace when entering initial defined function
                             (cond
                               (has-closure? "")
                               (else
                                 (string-append
                                   (st:->code trace)
                                   ;; TODO: probably needs brackets afterwards...
                                   (if has-loop? "\nloop:\n" "")
                                 ))))
                           body)
                         "  ")
                       "; \n"
                       "}\n"))
      formals*))))
  
(define cgen:mangle-global #f)

;; Convert a library name to string, so it can be 
;; appended to the identifiers it exports.
(define (import->string import)
  (foldr (lambda (id s) 
           (string-append "_" (mangle id) s)) 
         "" 
         (lib:list->import-set import)))

;; Identifier exported by another library
(define (mangle-exported-ident import-db ident error?)
  (let ((idb-entry (lib:idb:lookup import-db ident)))
    (cond
      ((not idb-entry)
       (if error?
           (error `(Unable to find a library importing ,ident))
           #f))
      (else
       (let ((suffix (import->string 
                       (lib:idb:entry->library-name idb-entry)))
             (prefix (mangle-global 
                       (lib:idb:entry->library-id idb-entry))))
         (string-append prefix suffix))))))

(define (mta:code-gen input-program 
                      program? 
                      lib-name 
                      lib-pass-thru-exports
                      import-db
                      globals
                      c-headers
                      required-libs
                      src-file)
  (set! *global-syms* (append globals (lib:idb:ids import-db)))
  (set! cgen:mangle-global
    (lambda (ident)
      (cond
        ;; Do not perform additional mangling for top-level globals
        ((and program? 
              (member ident globals))
         (mangle-global ident))
        ;; Identifier exported by the library being compiled
        ((or (member ident globals)
             (member ident lib-pass-thru-exports))
         (let ((suffix (import->string lib-name))
               (prefix (mangle-global ident)))
           (string-append prefix suffix)))
        ;; Identifier exported by another library
        (else
          (let ((idb-entry (lib:idb:lookup import-db ident)))
            (cond
              ((not idb-entry)
               (error `(Unable to find a library importing ,ident)))
              (else
               (let ((suffix (import->string 
                               (lib:idb:entry->library-name idb-entry)))
                     (prefix (mangle-global 
                               (lib:idb:entry->library-id idb-entry))))
                 (string-append prefix suffix)))))))))

  (let ((compiled-program-lst '())
        (compiled-program #f))
    ;; Compile program, using for-each to guarantee execution order,
    ;; since c-compile-program has side-effects.
    (for-each
      (lambda (expr)
        (set! compiled-program-lst
          (cons (c-compile-program expr src-file) compiled-program-lst)))
      input-program)

    ;; Get top-level string
    (set! compiled-program
      (foldr string-append "" (reverse compiled-program-lst)))

    (emit-c-arity-macros 0)
    (for-each 
      (lambda (h)
        (cond 
          ((and (string? h)
                (> (string-length h) 0)
                (equal? (string-ref h 0) #\<))
           (emit* "#include " h ""))
          (else
           (emit* "#include \"" h "\""))))
      c-headers)
    (emit "#include \"cyclone/types.h\"")

    ;; Globals defined in this module
    (for-each
        (lambda (global)
          (emits "object ")
          (emits (cgen:mangle-global (car global)))
          (emits " = NULL;\n"))
        *globals*)
    ;; "Pass-through"'s - exports from this module
    ;; that are actually defined by another.
    (for-each
        (lambda (global)
          (emits "object ")
          (emits (cgen:mangle-global global))
          (emits " = NULL;\n")

          (let ((extern (mangle-exported-ident import-db global #f)))
            (cond
              (extern
                (emits "extern object ")
                (emits extern)
                (emits ";\n")))))
        lib-pass-thru-exports)
    ;; Globals defined by another module
    (for-each
        (lambda (global)
          (emits "extern object ")
          (emits (cgen:mangle-global global))
          (emits ";\n"))
        (lib:idb:ids import-db))
    (emit "#include \"cyclone/runtime.h\"")

    (if program?
        (emit "#include \"cyclone/runtime-main.h\""))

    ;; Emit symbol definitions
    (for-each
        (lambda (sym)
            (emit* "defsymbol(" (mangle sym) ");"))
        *symbols*)

    ;; Emit lambdas:
    ; Print the prototypes:
    (for-each
     (lambda (l)
      (cond
        ((equal? 'precompiled-lambda (caadr l))
         (emit*
           "static void __lambda_" 
           (number->string (car l))
           (cadadr l)
           " ;"))
        ((equal? 'precompiled-inline-lambda (caadr l))
         (emit*
           "static object __lambda_" 
           (number->string (car l))
           (cadadr l)
           " ;"))
        ((member (car l) inline-lambdas)
         (emit*
           "static object __lambda_" 
           (number->string (car l)) "(void *data, "
           (cdadr l)
           ") ;"))
        (else
         (emit*
           "static void __lambda_" 
           (number->string (car l)) "(void *data, int argc, "
           (cdadr l)
           ") ;"))))
     lambdas)
    
    (emit "")
    
    ; Print the definitions:
    (for-each
     (lambda (l)
      (cond
       ((equal? 'precompiled-lambda (caadr l))
         (emit*
           "static void __lambda_" 
           (number->string (car l))
           (cadadr l)
           " {"
           (car (cddadr l))
           " }"
         ))
       ((equal? 'precompiled-inline-lambda (caadr l))
         (emit*
           "static object __lambda_" 
           (number->string (car l))
           (cadadr l)
           " {"
           (car (cddadr l))
           " }"
         ))
       ((member (car l) inline-lambdas)
        (emit ((caadr l) (string-append "__lambda_" (number->string (car l))))))
       (else
         (emit ((caadr l) (string-append "__lambda_" (number->string (car l))))))))
     lambdas)
  
    ; Emit inlinable function list
    (cond
      ((not program?)
        (emit* "void c_" (lib:name->string lib-name) "_inlinable_lambdas(void *data, int argc, closure _, object cont){ ")
        (let ((pairs '())
              (head-pair #f))
          (for-each
            (lambda (g)
              (let ((pair-sym (mangle (gensym 'pair))))
               (emits*
                   "make_pair(" pair-sym ", find_or_add_symbol(\"" (symbol->string (car g))
                   "\"), find_or_add_symbol(\"" (symbol->string (cdr g)) "\"));\n")
               (set! pairs (cons pair-sym pairs))))
            *global-inlines*)
            ;; Link the pairs
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
                   (if (not head-pair)
                       (set! head-pair (car cs)))
                   (loop (cons (string-append "make_pair(" (car cs) ", &" (car ps) ", NULL);\n") code)
                         (cdr ps)
                         (cdr cs)))
                  (else
                   (if (not head-pair)
                       (set! head-pair (car cs)))
                   (loop (cons (string-append "make_pair(" (car cs) ", &" (car ps) ", &" (cadr cs) ");\n") code)
                         (cdr ps) 
                         (cdr cs)))))
          (if head-pair
              (emit* "(((closure)cont)->fn)(data, 1, cont, &" head-pair ");")
              (emit* "(((closure)cont)->fn)(data, 1, cont, NULL);"))
          (emit* " } "))
      ))

    ; Emit entry point
    (cond
      (program?
        (emit "static void c_entry_pt_first_lambda(void *data, int argc, closure cont, object value);")
        (for-each
          (lambda (lib-name)
            (emit* "extern void c_" (lib:name->string lib-name) "_entry_pt(void *data, int argc, closure cont, object value);"))
          required-libs)
        (emit "static void c_entry_pt(data, argc, env,cont) void *data; int argc; closure env,cont; { "))
      (else
        (emit* "void c_" (lib:name->string lib-name) "_entry_pt_first_lambda(data, argc, cont,value) void *data; int argc; closure cont; object value;{ ")
        ; DEBUG (emit (string-append "printf(\"init " (lib:name->string lib-name) "\\n\");"))
      ))

    ;; Set global-changed indicator
    (emit "Cyc_set_globals_changed((gc_thread_data *)data);")

    ;; Initialize symbols
    (for-each
        (lambda (sym)
            (emit* 
                "  quote_" (mangle sym) " = find_or_add_symbol(\"" 
                (symbol->string sym) "\");"))
        *symbols*)

    ;; Initialize global table
    (for-each
      (lambda (global)
        (emits "\n  add_global((object *) &")
        (emits (cgen:mangle-global (car global)))
        (emits ");"))
      *globals*)
    (emit "")

    ;; Initialize symbol table
    (for-each
        (lambda (sym)
            (emit* "  add_symbol(quote_" (mangle sym) ");"))
        *symbols*)

    ;; Initialize globals
    (let* ((prefix "  ")
           (emit-global
             (lambda (global)
               (emits (c:allocs->str2 (c:allocs (caddr global)) prefix " \n"))
               (emits prefix)
               (emits (cgen:mangle-global (car global)))
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

        ;; Expose list of inlinable lambda functions
        (when (not program?)
          (let ( ;(cvar-sym (mangle (gensym 'cvar)))
                (pair-sym (mangle (gensym 'pair)))
                (clo-sym (mangle (gensym 'clo)))
                (fnc (string-append
                       "c_" (lib:name->string lib-name) "_inlinable_lambdas")))
           (emits* 
               "  mclosure0(" clo-sym ", " fnc "); "
              ; "  make_cvar(" cvar-sym 
              ; ", (object *)&" fnc ");"
               )
           (emits*
               "make_pair(" pair-sym ", find_or_add_symbol(\"" fnc
               "\"), &" clo-sym ");\n")
           (set! pairs (cons pair-sym pairs))))
        ;; END

        (for-each
          (lambda (g)
            (let ((cvar-sym (mangle (gensym 'cvar)))
                  (pair-sym (mangle (gensym 'pair))))
             (emits* 
                 "  make_cvar(" cvar-sym 
                 ", (object *)&" (cgen:mangle-global (car g)) ");")
             (emits*
                 "make_pair(" pair-sym ", find_or_add_symbol(\"" (symbol->string (car g))
                 "\"), &" cvar-sym ");\n")
             (set! pairs (cons pair-sym pairs))
          ))
          *globals*)
        (for-each
          (lambda (g)
            (let ((idb-entry (lib:idb:lookup import-db g)))
              (if idb-entry
                  (emits* 
                    (cgen:mangle-global g) " = " 
                    (mangle-exported-ident import-db g #f)
                    ";\n"))))
          lib-pass-thru-exports)
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
               (if (not head-pair)
                   (set! head-pair (car cs)))
               (loop (cons (string-append "make_pair(" (car cs) ", &" (car ps) ",Cyc_global_variables);\n") code)
                     (cdr ps)
                     (cdr cs)))
              (else
               (if (not head-pair)
                   (set! head-pair (car cs)))
               (loop (cons (string-append "make_pair(" (car cs) ", &" (car ps) ", &" (cadr cs) ");\n") code)
                     (cdr ps) 
                     (cdr cs)))))
        (if head-pair
            (emit*
              "Cyc_global_variables = &" head-pair ";")))

    (cond
      (program?
        ;; Emit code to initialize each module (compiled Scheme library)
        (let ((this-clo "c_done")
              (prev-clo "c_done"))
          (emit*
              "mclosure1(" this-clo
              ", c_entry_pt_first_lambda, &" prev-clo ");")
          (for-each
            (lambda (lib-name)
              (set! prev-clo this-clo)
              (set! this-clo (mangle (gensym "c")))
              (emit* 
                  "mclosure1(" this-clo
                  ", c_" (lib:name->string lib-name) "_entry_pt"
                  ", &" prev-clo ");")
            )
            (reverse required-libs)) ;; Init each lib's dependencies 1st
          (emit* 
            ;; Start cont chain, but do not assume closcall1 macro was defined
            "(" this-clo ".fn)(data, 0, &" this-clo ", &" this-clo ");")
          (emit "}")
          (emit "static void c_entry_pt_first_lambda(void *data, int argc, closure cont, object value) {")
          ; DEBUG (emit (string-append "printf(\"init first lambda\\n\");"))
          (emit compiled-program)
          (emit ";")))
      (else
        ;; Do not use closcall1 macro as it might not have been defined
        (emit "cont = ((closure1_type *)cont)->element;")
        (emit* 
            "(((closure)"
            (cgen:mangle-global (lib:name->symbol lib-name)) 
            ")->fn)(data, 1, cont, cont);")

        (emit* "}")
        (emit* "void c_" (lib:name->string lib-name) "_entry_pt(data, argc, cont,value) void *data; int argc; closure cont; object value;{ ")
        (emit* "  register_library(\""
               (lib:name->unique-string lib-name)
               "\");")
        (if (null? lib-pass-thru-exports)
            (emit* "  c_" (lib:name->string lib-name) "_entry_pt_first_lambda(data, argc, cont,value);")
            ; GC to ensure objects are moved when exporting exports.
            ; Otherwise there will be broken hearts :(
            (emit*
              "  mclosure1(clo, c_" (lib:name->string lib-name) "_entry_pt_first_lambda, ((closure1_type *)cont)->element);\n"
              "  object buf[1]; buf[0] = cont;\n"
              "  GC(data, (closure)&clo, buf, 1);\n"))
      ))

    (emit "}")
    (if program?
      (emit *c-main-function*))))


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

))
