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
          (cyclone foreign)
          (scheme cyclone primitives)
          (scheme cyclone transforms)
          (scheme cyclone ast)
          (scheme cyclone cps-optimizations)
          (scheme cyclone util)
          (scheme cyclone libraries))
  (export
    mta:code-gen
    autogen
    autogen:defprimitives 
    autogen:primitive-procedures 
    ;;c-compile-program
    emit
    emit*
    emits
    emits*
    emit-newline
    ;; Helpers
    self-closure-call?)
  (inline
    global-not-lambda?
    global-lambda?
    c:num-args
    c:allocs
    st:->var)
  (begin

(define *cgen:track-call-history* #t)
(define *cgen:use-unsafe-prims* #f)
(define *optimize-well-known-lambdas* #f)

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
       (emit (c-macro-continue-or-gc arity))
       (emit (c-macro-return-direct arity))
       (emit (c-macro-return-direct-with-closure arity))
       (when *optimize-well-known-lambdas*
         (emit (c-macro-return-direct-with-object arity))) ))
    (emit-c-arity-macros (+ arity 1))))

;; Generate macros to call a closures
(define (c-macro-return-closcall num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
     ;;"/* Check for GC, then call given continuation closure */\n"
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

;; Generate macros invoke a GC if necessary, otherwise do nothing.
;; This will be used to support C iteration.
(define (c-macro-continue-or-gc num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
     ;;"/* Check for GC, then call given continuation closure */\n"
      "#define continue_or_gc" n "(td, clo" args ") { \\\n"
      " char *top = alloca(sizeof(char)); \\\n" ; TODO: consider speeding up by passing in a var already allocated
      " if (stack_overflow(top, (((gc_thread_data *)data)->stack_limit))) { \\\n"
      "     object buf[" n "]; " arry-assign "\\\n"
      "     GC(td, clo, buf, " n "); \\\n"
      "     return; \\\n"
      " } else {\\\n"
      "     continue;\\\n"
      " } \\\n"
      "}\n")))

;; Generate macros to directly call a lambda function
(define (c-macro-return-direct num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
     ;;"/* Check for GC, then call C function directly */\n"
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

(define (c-macro-return-direct-with-closure num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
     ;;"/* Check for GC, then call C function directly */\n"
      "#define return_direct_with_clo" n "(td, clo, _fn" args ") { \\\n"
      " char top; \\\n"
      " if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \\\n"
      "     object buf[" n "]; " arry-assign "\\\n"
      "     GC(td, clo, buf, " n "); \\\n"
      "     return; \\\n"
      " } else { \\\n"
      "     (_fn)(td, " n ", (closure)(clo)" args "); \\\n"
      " }}\n")))

;; Generate hybrid macros that can call a function directly but also receives
;; an object instead of a closure (closure optimized-out)
(define (c-macro-return-direct-with-object num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (arry-assign (c-macro-array-assign num-args "buf" "a")))
    (string-append
     ;;"/* Check for GC, then call C function directly */\n"
      "#define return_direct_with_obj" n "(td, clo, _clo_fn, _fn" args ") { \\\n"
      " char top; \\\n"
      " if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \\\n"
      "     object buf[" n "]; " arry-assign "\\\n"
      "     mclosure1(c1, (function_type) _clo_fn, clo); \\\n"
      "     GC(td, (closure)(&c1), buf, " n "); \\\n"
      "     return; \\\n"
      " } else { \\\n"
      "     (_fn)(td, " n ", (closure)(clo)" args "); \\\n"
      " }}\n")))

(define (c-macro-closcall num-args)
  (let ((args (c-macro-n-prefix num-args ",a"))
        (n (number->string num-args))
        (n-1 (number->string (if (> num-args 0) (- num-args 1) 0)))
        (wrap (lambda (s) (if (> num-args 0) s ""))))
    (string-append
      "#define closcall" n "(td, clo" args ") \\\n"
        (wrap (string-append "if (obj_is_not_closure(clo)) { \\\n"
                             "   Cyc_apply(td, " n-1 ", (closure)(a1), clo" (if (> num-args 1) (substring args 3 (string-length args)) "") "); \\\n"
                             "}"))
        (wrap " else { \\\n")
        "   ((clo)->fn)(td, " n ", clo" args ")"
        (wrap ";\\\n}"))))

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
          (null? (cdr trace))
          (not *cgen:track-call-history*))
    ""
    (string-append 
      "Cyc_st_add(data, \""
      (car trace) 
      ":" 
      ;; TODO: escape backslashes
      (symbol->string (cdr trace))
      "\");\n")))

(define (st:->var trace)
  (cdr trace))
;; END st helpers

;;; Compilation routines.

;; Return generated code that also requests allocation of C variables on stack
(define (c:code/vars str cvars)
  (list str
        cvars))

;; Return generated code with no C variables allocated on the stack
(define (c:code str) (c:code/vars str (list)))

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
  (c:code/vars 
    (string-append (c:body cp1) (c:body cp2))
    (append (c:allocs cp1) (c:allocs cp2))))

(define (c:append/prefix prefix cp1 cp2)
  (c:code/vars 
    (string-append prefix (c:body cp1) (c:body cp2))
    (append (c:allocs cp1) (c:allocs cp2))))

(define (c:serialize cp prefix)
  (let* ((body (c:body cp))
         (blen (string-length body)))
    (string-append
     (c:allocs->str (c:allocs cp) prefix)
     prefix
     body
     (if (and (> blen 0)
              (not (eq? #\; (string-ref body (- blen 1))))) ; last char
         ";"
         ""))))

;; c-compile-program : exp -> string
(define (c-compile-program exp src-file)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble "cont" -1 (list src-file) #t)))
    ;; (write `(DEBUG ,body))
    (string-append 
     preamble 
     (c:serialize body "  "))))

;; c-compile-exp : exp (string -> void) -> string
;;
;; exp - expression to compiler
;; append-preamble - ??
;; cont - name of the next continuation
;;        this is experimental and probably needs refinement
;; ast-id - The AST lambda ID of the function containing the expression
;; trace - trace information. presently a pair containing:
;;         * source file
;;         * function name (or NULL if none)
;; cps? - Determine whether to compile using continuation passing style.
;;        Normally this is always enabled, but sometimes a function has a
;;        version that can be inlined (as an optimization), so this will
;;        be set to false to change the type of compilation.
;;        NOTE: this field is not passed everywhere because a lot of forms
;;              require CPS, so this flag is not applicable to them.
(define (c-compile-exp exp append-preamble cont ast-id trace cps?)
  (cond
   ;; Special case - global function w/out a closure. Create an empty closure
    ((ast:lambda? exp)
     (c-compile-exp
      `(%closure ,exp)
       append-preamble 
       cont 
       ast-id
       trace 
       cps?))
    ;; Core forms:
    ((const? exp)       (c-compile-const exp (alloca? ast-id trace) #f)) ;; TODO: OK to hardcode immutable to false here??
    ((prim?  exp)       
     ;; TODO: this needs to be more refined, probably w/a lookup table
     (c:code (string-append "primitive_" (mangle exp))))
    ((ref?   exp)       (c-compile-ref exp))
    ((quote? exp)       (c-compile-quote exp (alloca? ast-id trace)))
    ((if? exp)          (c-compile-if exp append-preamble cont ast-id trace cps?))

    ;; IR (2):
    ((tagged-list? '%closure exp)
     (c-compile-closure exp append-preamble cont ast-id trace cps?))
    ;; Global definition
    ((define? exp)
     (c-compile-global exp append-preamble cont trace))
    ((define-c? exp)
     (c-compile-raw-global-lambda exp append-preamble cont trace))
    
    ;; Application:
    ((app? exp)         (c-compile-app exp append-preamble cont ast-id trace cps?))
    (else               (error "unknown exp in c-compile-exp: " exp))))

(define (c-compile-quote qexp use-alloca)
  (let ((exp (cadr qexp)))
    (c-compile-scalars exp use-alloca #t)))

;; Emit code to set an object's immutable field
;;
;; Params:
;;  cvar - String - Name of C variable containing the object.
;;  use-alloca - Boolean - Is C var dynamically allocated?
;;  immutable - Boolean - Is object immutable?
;;
;; Returns a string containing generated C code
(define (c-set-immutable-field cvar use-alloca immutable)
  (cond
    ((and immutable use-alloca)
     (string-append cvar "->hdr.immutable = 1;"))
    ((and immutable (not use-alloca))
     (string-append cvar ".hdr.immutable = 1;"))
    (else ""))) ;; Mutable (default), no need to set anything

(define (c-compile-scalars args use-alloca immutable)
  (letrec (
    (addr-op (if use-alloca "" "&"))
    ;; (deref-op (if use-alloca "->" "."))
    (c-make-macro (if use-alloca "alloca_pair" "make_pair"))
    (num-args 0)
    (create-cons
      (lambda (cvar a b)
        (c:code/vars
         (string-append 
           c-make-macro "(" cvar "," (c:body a) "," (c:body b) ");"
           (c-set-immutable-field cvar use-alloca immutable))
         (append (c:allocs a) (c:allocs b)))))
    (_c-compile-scalars 
     (lambda (args)
       (cond
        ((null? args)
           (c:code "NULL"))
        ((not (pair? args))
         (c-compile-const args use-alloca immutable))
        (else
           (let* ((cvar-name (mangle (gensym 'c)))
                  (cell (create-cons
                          cvar-name
                          (c-compile-const (car args) use-alloca immutable) 
                          (_c-compile-scalars (cdr args)))))
             (set! num-args (+ 1 num-args))
             (c:code/vars
                (string-append addr-op cvar-name)
                (append
                  (c:allocs cell)
                  (list (c:body cell))))))))))
  (c:tuple/args
    (_c-compile-scalars args) 
    num-args)))

(define (c-compile-vector exp use-alloca immutable)
  (letrec ((cvar-name (mangle (gensym 'vec)))
           (len (vector-length exp))
           (ev-name (mangle (gensym 'e)))
           (elem-decl 
             (if use-alloca
                 (string-append "object *" ev-name " = (object *)alloca(sizeof(object) * " 
                                              (number->string len) ");")
                 (string-append "object " ev-name " [" (number->string len) "];\n")))
           (addr-op (if use-alloca "" "&"))
           (deref-op (if use-alloca "->" "."))
           (c-make-macro (if use-alloca "alloca_empty_vector" "make_empty_vector"))
           ;; Generate code for each member of the vector
           (loop 
            (lambda (i code)
              (if (= i len)
                code
                (let ((idx-code (c-compile-const (vector-ref exp i) use-alloca immutable)))
                  (loop 
                    (+ i 1)
                    (c:code/vars
                     ;; The vector's C variable
                      (c:body code)
                      ;; Allocations
                      (append
                       (c:allocs code) ;; Vector alloc
                       (c:allocs idx-code) ;; Member alloc at index i
                       (list ;; Assign this member to vector
                          (string-append 
                            cvar-name deref-op "elements[" (number->string i) "] = "
                            (c:body idx-code)
                            ";"))))))))))
    (cond
      ((zero? len)
        (c:code/vars
            (string-append addr-op cvar-name) ; Code is just the variable name
            (list ; Allocate empty vector
              (string-append 
                c-make-macro "(" cvar-name ");"
                (c-set-immutable-field cvar-name use-alloca immutable)))))
      (else
        (let ((code
                (c:code/vars
                  (string-append addr-op cvar-name) ; Code body is just var name
                  (list ; Allocate the vector
                    (string-append 
                      elem-decl
                      c-make-macro "(" cvar-name ");"
                      cvar-name deref-op "num_elements = " (number->string len) ";"
                      cvar-name deref-op "elements = (object *)" ev-name ";"
                      (c-set-immutable-field cvar-name use-alloca immutable)
                      )))))
        (loop 0 code))))))

(define (c-compile-bytevector exp use-alloca immutable)
  (letrec ((cvar-name (mangle (gensym 'vec)))
           (len (bytevector-length exp))
           (addr-op (if use-alloca "" "&"))
           (deref-op (if use-alloca "->" "."))
           (c-make-macro (if use-alloca "alloca_empty_bytevector" "make_empty_bytevector"))
           ;; Generate code for each member of the vector
           (loop 
            (lambda (i code)
              (if (= i len)
                code
                (let ((byte-val (number->string (bytevector-u8-ref exp i))))
                  (loop 
                    (+ i 1)
                    (c:code/vars
                     ;; The bytevector's C variable
                      (c:body code)
                      ;; Allocations
                      (append
                       (c:allocs code) ;; Vector alloc
                       (list ;; Assign this member to vector
                          (string-append 
                            cvar-name deref-op "data[" (number->string i) "] = (unsigned char)"
                            byte-val
                            ";"))))))))))
    (cond
      ((zero? len)
        (c:code/vars
            (string-append addr-op cvar-name) ; Code is just the variable name
            (list ; Allocate empty vector
              (string-append 
                c-make-macro "(" cvar-name ");"
                (c-set-immutable-field cvar-name use-alloca immutable)
              ))))
      (else
        (let ((code
                (c:code/vars
                  (string-append addr-op cvar-name) ; Code body is just var name
                  (list ; Allocate the vector
                    (string-append 
                      c-make-macro "(" cvar-name ");"
                      cvar-name deref-op "len = " (number->string len) ";"
                      cvar-name deref-op "data = alloca(sizeof(char) * " 
                                         (number->string len) ");"
                      (c-set-immutable-field cvar-name use-alloca immutable)
                    )))))
        (loop 0 code))))))

(define (c-compile-string exp use-alloca immutable)
  (let ((cvar-name (mangle (gensym 'c))))
    (cond
      (use-alloca
       (let ((tmp-name (mangle (gensym 'tmp)))
             (blen (number->string (string-byte-length exp))))
        (c:code/vars
          (string-append "" cvar-name) ; Code is just the variable name
          (list     ; Allocate integer on the C stack
            (string-append 
              "object " cvar-name ";\n "
              "alloc_string(data," 
              cvar-name 
              ", " 
              blen
              ", " 
              (number->string (string-length exp))
              ");\n"
              "char " tmp-name "[] = "
              (->cstr exp) 
              ";\n"
              "memcpy(((string_type *)" cvar-name ")->str, " tmp-name "," blen ");\n"
              "((string_type *)" cvar-name ")->str[" blen "] = '\\0';"
              (c-set-immutable-field 
                (string-append
                  "((string_type *)" cvar-name ")")
                use-alloca immutable)
            )))))
      (else
        (c:code/vars
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
              ");"
              (c-set-immutable-field cvar-name use-alloca immutable)
            )))))))

;; c-compile-const : const-exp -> c-pair
;;
;; Typically this function is used to compile constant values such as
;; a single number, boolean, etc. However, it can be passed a quoted
;; item such as a list, to compile as a literal.
;;
;; exp - Expression to compile
;; use-alloca - Should C objects be dynamically allocated on the stack?
;; immutable - Should C object be flagged as immutable?
(define (c-compile-const exp use-alloca immutable)
  (cond
    ((null? exp)
     (c:code "NULL"))
    ((eq? (void) exp) ;; Poor man's (void?)
     (c:code "Cyc_VOID"))
    ((pair? exp)
     (c-compile-scalars exp use-alloca immutable))
    ((vector? exp)
     (c-compile-vector exp use-alloca immutable))
    ((bytevector? exp)
     (c-compile-bytevector exp use-alloca immutable))
    ((bignum? exp)
      (let ((cvar-name (mangle (gensym 'c)))
            (num2str (cond
                       (else
                         (number->string exp)))))
        (c:code/vars
         (string-append "" cvar-name) ; Code is just the variable name
            (list     ; Allocate pointer on the C stack
              (string-append 
                "alloc_bignum(data, " cvar-name "); "
                ;; TODO: need error checking, this is just a first cut:
                "BIGNUM_CALL(mp_read_radix(&bignum_value(" cvar-name "), \"" num2str "\", 10));")))))
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
             (addr-op (if use-alloca "" "&"))
             (c-make-macro (if use-alloca "alloca_complex_num" "make_complex_num")))
        (c:code/vars
            (string-append addr-op cvar-name) ; Code is just the variable name
            (list     ; Allocate on the C stack
              (string-append 
                c-make-macro "(" cvar-name ", " rnum ", " inum ");")))))
    ((and (integer? exp)
          (exact? exp))
     (c:code (string-append "obj_int2obj(" 
               (number->string exp) ")")))
    ((real? exp)
      (let ((cvar-name (mangle (gensym 'c)))
            (num2str (cond
                      ;; The following two may not be very portable,
                       ;; may be better to use C99:
                       ((nan? exp) "(0./0.)")
                       ((infinite? exp) "(1./0.)")
                       (else
                         (number->string exp))))
            (addr-op (if use-alloca "" "&"))
            (c-make-macro (if use-alloca "alloca_double" "make_double")))
        (c:code/vars
         (string-append addr-op cvar-name) ; Code is just the variable name
            (list     ; Allocate on the C stack
              (string-append 
                c-make-macro "(" cvar-name ", " num2str ");")))))
    ((boolean? exp) 
      (c:code (string-append
                (if exp "boolean_t" "boolean_f"))))
    ((char? exp)
     (c:code (string-append "obj_char2obj(" 
               (number->string (char->integer exp)) ")")))
    ((string? exp)
     (c-compile-string exp use-alloca immutable))
    ((symbol? exp)
     (allocate-symbol exp)
     (c:code (string-append "quote_" (mangle exp))))
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

(define *use-alloca* #f)

(define (set-use-alloca! v)
  (set! *use-alloca* v))

;; Use alloca() for stack allocations?
(define (alloca? ast-id trace)
  (or *use-alloca*
      (let ((adbf:fnc (adb:get/default ast-id #f)))
        (or
          ;; Newer logic
          (and adbf:fnc 
               (adbf:calls-self? adbf:fnc))
          ;; Older direct recursive logic 
          (and
            (pair? trace)
            (not (null? (cdr trace)))
            (adbv:direct-rec-call? (adb:get (cdr trace))))))))

;; c-compile-prim : prim-exp -> string -> string
(define (c-compile-prim p cont ast-id)
  (let* ((use-alloca? (alloca? ast-id #f))
         (c-func 
           (if (prim:udf? p)
               (string-append
                 "((inline_function_type)
                   ((closure)"
                    (cgen:mangle-global p)
                 ")->fn)")
               (prim->c-func p use-alloca? *cgen:use-unsafe-prims*)))
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
         (tptr-comma 
          (cond
           ((and tptr-type use-alloca?) tdata-comma)
           (tptr-type (string-append tdata-comma "&"))
           (else "")))
         (tptr (cond
                (tptr-type (mangle (gensym 'local)))
                (else "")))
         (tptr-decl
          (cond 
           ((and tptr-type use-alloca?) (string-append "object " tptr " = alloca(sizeof(" tptr-type ")); "))
            (tptr-type (string-append tptr-type " " tptr "; "))
            (else "")))
         (c-var-assign 
            (lambda (type)
              (let ((cv-name (mangle (gensym 'c))))
                (c:code/vars 
                  (string-append 
                    (if (or (prim:cont? p) 
                            (equal? (prim/c-var-assign p) "object")
                            (prim/c-var-pointer p) ; Assume returns object
                            (prim->c-func-uses-alloca? p use-alloca?))
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
           (c:code/vars
            (if (or (prim:allocates-object? p use-alloca?)
                    (prim->c-func-uses-alloca? p use-alloca?))
                cv-name ; Already a pointer
                (string-append "&" cv-name)) ; Point to data
            (list
                (string-append c-func "(" cv-name tdata-comma tdata)))))
     (else
        (c:code/vars 
          (string-append c-func "(" tdata tptr-comma tptr)
          (list tptr-decl))))))

;; END primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; self-closure-call? :: sexp -> symbol -> integer -> boolean
;;
;; Determine whether we have a closure call of the form:
;;  (%closure-ref
;;     (cell-get (%closure-ref self$249 1))
;;     0)
;;
;; Parameters:
;; ast - S-expression to analyze
;; self - Identifier for the function's "self" closure
;; closure-index - Index of the function's "self" closure in outer closure
(define (self-closure-call? ast self closure-index)
  ;; (trace:error `(JAE self-closure-call? ,ast ,self ,closure-index))
  (and-let* (((tagged-list? '%closure-ref ast))
             ((tagged-list? 'cell-get (cadr ast)))
             (inner-cref (cadadr ast))
             ((tagged-list? '%closure-ref inner-cref))
             (equal? self (cadr inner-cref))
             ((equal? 0 (caddr ast)))
             ((equal? closure-index (caddr inner-cref))))
    #t))

;; c-compile-ref : ref-exp -> string
(define (c-compile-ref exp)
  (c:code 
    (if (member exp *global-syms*)
      (cgen:mangle-global exp)
      (mangle exp))))

;; c-compile-args : list[exp] (string -> void) -> string
(define (c-compile-args args append-preamble prefix cont ast-id trace cps?)
  (letrec ((num-args 0)
           (cp-lis '())
         (_c-compile-args 
          (lambda (args append-preamble prefix cont)
            (cond
             ((not (pair? args))
              (c:code ""))
             (else
              ;; (trace:debug `(c-compile-args ,(car args)))
              (let ((cp (c-compile-exp (car args) 
                          append-preamble cont ast-id trace cps?)))
                (set! num-args (+ 1 num-args))
                (set! cp-lis (cons cp cp-lis))
                (c:append/prefix
                  prefix 
                  cp
                  (_c-compile-args (cdr args) 
                    append-preamble ", " cont))))))))
    ;; Pass back a container with:
    ;; - Appened body (string)
    ;; - Appended allocs (string)
    ;; - Number of args (numeric)
    ;; - Remaining args - Actual CP objects (lists of body/alloc) from above
  (append
    (c:tuple/args
      (_c-compile-args args 
        append-preamble prefix cont)
      num-args)
    (reverse cp-lis))))

;; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble cont ast-id trace cps?)
  ;;(trace:info `(c-compile-app: ,exp ,trace))
  (let (($tmp (mangle (gensym 'tmp))))
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
      (cond
        ((ast:lambda? fun)
         (let* ((lid (allocate-lambda fun (c-compile-lambda fun trace #t)))
                ;; TODO: pass in free vars? may be needed to track closures
                ;; properly, wait until this comes up in an example
                (this-cont (string-append "__lambda_" (number->string lid)))
                (cgen 
                  (c-compile-args
                     args 
                     append-preamble 
                     ""
                     this-cont
                     ast-id
                     trace
                     cps?))
                (num-cargs (c:num-args cgen)))
           (set-c-call-arity! num-cargs)
           (c:code
             (string-append
               (c:allocs->str (c:allocs cgen))
               "return_direct" (number->string num-cargs) 
               "(data," this-cont
               (if (> num-cargs 0) "," "") ; TODO: how to propagate continuation - cont " "
                  (c:body cgen) ");"))))

        ;; Direct recursive call of top-level function
        ((and (pair? trace)
              (not (null? (cdr trace)))
              (adbv:direct-rec-call? (adb:get (cdr trace)))
              (tagged-list? '%closure-ref fun)
              (equal? (cadr fun) (cdr trace)) ; Needed?
              (equal? (car args) (cdr trace))
              ;; Make sure continuation is not a lambda, because
              ;; that means a closure may be allocated
              (ref? (cadr args)))
         (let* ((cgen-lis
                  (map 
                    (lambda (e)
                     (c-compile-exp e append-preamble "" ast-id "" cps?))
                    (cddr args))) ; Skip the closure
                (cgen-allocs 
                  (apply string-append 
                    (map (lambda (a) (c:allocs->str (c:allocs a))) cgen-lis)))

                (parent-fnc (adbv:assigned-value (adb:get (cdr trace))))
                (parent-args 
                  (cdr ; Skip continuation
                    (ast:lambda-args 
                      (if (pair? parent-fnc) 
                          (car parent-fnc)
                          parent-fnc))))
                (cgen-body
                  (apply
                    string-append
                    (map
                      (lambda (arg body-exp)
                        (if (equal? (mangle arg) (c:body body-exp))
                            "" ;; Do nothing
                            (string-append
                              (mangle arg)
                              " = "
                              (c:body body-exp)
                              ";")))
                      parent-args
                      cgen-lis))))
           ;;(trace:info `(loop ,args ,(cadr args) ,cgen-lis ,parent-args))
           (c:code
             (string-append
              cgen-allocs ; (c:allocs->str (c:allocs cgen))
               "\n"
               cgen-body ; TODO: (c:body cgen) ; TODO: re-assign function args, longer-term using temp variables
               "\n"
               "continue;"))))
         
        ((eq? 'Cyc-foreign-code fun)
         (c:code/vars 
           (string-append 
             "")
           args))

        ((eq? 'Cyc-foreign-value fun)
         (let ((kons (c->scm (car args) (cadr args))))
           (c:code/vars
             (cdr kons)
             (list (car kons)))))

        ((prim? fun)
         (let* ((c-fun 
                 (c-compile-prim fun cont ast-id))
                (c-args
                 (c-compile-args args append-preamble "" "" ast-id trace cps?))
                (num-args (length args))
                (num-args-str
                  (string-append 
                    (number->string num-args)
                    (if (> num-args 0) "," "")))
                (c-args* (if (prim:arg-count? fun)
                             (c:append (c:code num-args-str) c-args)
                             c-args)))
            ;; Emit symbol when mutating global variables, so we can look 
            ;; up the cvar
            (when (eq? 'set-global! fun)
              (let* ((ident (cadr args))
                     (mangled (string-append "\"" (cgen:mangle-global ident) "\""))
                     (all-args (string-split (car c-args) #\,))
                     (new-all-args (string-join (cons mangled (cdr all-args)) ","))
                    )
                (set-car! c-args* new-all-args)
                (set-car! (cadddr c-args*) mangled)
                ;(trace:debug `(JAE set-global args are ,c-args ,args mangled ))
                ;; Example c-args:
                ;;("quote__121pare_125, __glo__121pare_125, r_73558_731010_731308_731412" () 3 ("quote__121pare_125" () 0) ("__glo__121pare_125" ()) ("r_73558_731010_731308_731412" ()))
                ))

            (if (prim/cvar? fun)
              ;; Args need to go with alloc function
              (c:code/vars
                (c:body c-fun)
                (append
                  (c:allocs c-args*) ; fun alloc depends upon arg allocs
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
                            (prim:cont/no-args? fun)
                            (and (prim:udf? fun)
                                 (zero? num-args)))
                      c-fun
                      (c:append c-fun (c:code ", "))))
                  c-args*)
                (c:code ")")))))

        ((equal? '%closure-ref fun)
         (c:code (apply string-append (list
            (c-compile-closure-element-ref 
              ast-id
              (car args)
              (number->string (- (cadr args) 1)))
            ;;"("
            ;;; TODO: probably not the ideal solution, but works for now
            ;;"(closureN)"
            ;; (mangle (car args))
            ;;")->elements["
            ;; (number->string (- (cadr args) 1))"]"
            ))))

        ;; TODO: may not be good enough, closure app could be from an element
        ((tagged-list? '%closure-ref fun)
         (let* ((cfun (c-compile-args (list (car args)) append-preamble "  " cont ast-id trace cps?))
                (this-cont (c:body cfun))
                (cargs (c-compile-args (cdr args) append-preamble "  " this-cont ast-id trace cps?))
                (raw-cargs (cdddr cargs)) ; Same as above but with lists instead of appended strings
                (num-cargs (c:num-args cargs)))
           (cond
             ((not cps?)
              (c:code 
                (string-append
                  (c:allocs->str (c:allocs cfun) "\n")
                  (c:allocs->str (c:allocs cargs) "\n")
                  "return_copy(ptr,"
                  (c:body cargs)
                  ");")))
             (else
              ;;TODO: Consolidate with corresponding %closure code??
              (set-c-call-arity! (c:num-args cargs))
              (let* ((wkf (well-known-lambda (car args)))
                     (fnc (if wkf (adb:get/default (ast:lambda-id wkf) #f) #f))
                     (adbf:fnc (adb:get/default ast-id #f)))
                (cond
                  ;; Handle recursive calls via iteration, if possible
                  ((and adbf:fnc
                        ;#f ;; TODO: temporarily disabled
                        (adbf:calls-self? adbf:fnc)
                        (self-closure-call? 
                          fun 
                          (car (adbf:all-params adbf:fnc))
                          (adbf:self-closure-index adbf:fnc)))
                    (let* ((params (map mangle (cdr (adbf:all-params adbf:fnc))))
                           (tmp-params (map
                                         (lambda (param)
                                           (string-append "tmp_" param))
                                         params))
                           (args (map car raw-cargs))
                           (reassignments 
                            ;; TODO: may need to detect cases where an arg is reassigned before
                            ;; another one is assigned to that arg's old value, for example:
                            ;;   a = 1, b = 2, c = a
                            ;; In this case the code would need to assign to a temporary variable
                            ;;
                            ;; Right now we just play it safe and always assign to temporary variables,
                            ;; even when we don't need to. I suppose in theory the C compiler can
                            ;; figure that out (??) but it would be cleaner overall if we could here.
                            ;; Something to consider for the future.
                             (apply string-append
                              (map
                                (lambda (param arg)
                                  (cond
                                   ;; TODO: with tmps this is not really applicable anymore:
                                   ((equal? param arg) "") ; No need to reassign
                                    (else
                                      (string-append
                                        param " = " arg ";\n"))))
                                tmp-params
                                args)))
                           (swap-tmps
                             (apply string-append
                               (map
                                (lambda (p tmp)
                                  (string-append " " p " = " tmp "; "))
                                params tmp-params))))
                      ;; (trace:error `(JAE ,fun ,ast-id ,params ,args (c:num-args cargs)))
                    (c:code/vars 
                      (string-append
                        (c:allocs->str (c:allocs cfun) "\n")
                        (c:allocs->str (c:allocs cargs) "\n")
                        reassignments
                        swap-tmps
                        ;; TODO: consider passing in a "top" instead of always calling alloca in macro below:
                        "continue_or_gc" (number->string (c:num-args cargs))
                        "(data,"
                        (mangle (car (adbf:all-params adbf:fnc))) ;; Call back into self after GC
                        (if (> (c:num-args cargs) 0) "," "")
                        (string-join params ", ")
                        ");")
                      (map 
                        (lambda (param)
                          (string-append " object " param "; "))
                        tmp-params))))
                        
                  ((and wkf fnc
                        *optimize-well-known-lambdas*
                        (adbf:well-known fnc) ; not really needed
                        (equal? (adbf:closure-size fnc) 1))
                   (let* ((lid (ast:lambda-id wkf))
                          (c-lambda-fnc-str (string-append "__lambda_" (number->string lid)))
                          (c-lambda-fnc-gc-ret-str (string-append "__lambda_gc_ret_" (number->string lid))))
                     (c:code
                       (string-append
                          (c:allocs->str (c:allocs cfun) "\n")
                          (c:allocs->str (c:allocs cargs) "\n")
                          "return_direct_with_obj" (number->string num-cargs)
                          "(data,"
                          this-cont
                          ","
                          c-lambda-fnc-gc-ret-str
                          ","
                          c-lambda-fnc-str
                          (if (> num-cargs 0) "," "")
                          (c:body cargs)
                          ");"))))
                  ;; TODO: here  and in other case, if well-known but closure size does not match, use
                  ;; other macro to at least call out the __lambda_ function directly. seemed to
                  ;; speed up C compile times (let's test that!)
                  ;; "#define return_direct_with_clo" n "(td, clo, _fn" args ") { \\\n"
                  ((and wkf fnc)
                   (let* ((lid (ast:lambda-id wkf))
                          (c-lambda-fnc-str (string-append "__lambda_" (number->string lid))))
                     (c:code
                       (string-append
                          (c:allocs->str (c:allocs cfun) "\n")
                          (c:allocs->str (c:allocs cargs) "\n")
                          "return_direct_with_clo" (number->string num-cargs)
                          "(data,"
                          this-cont
                          ","
                          c-lambda-fnc-str
                          (if (> num-cargs 0) "," "")
                          (c:body cargs)
                          ");"))))
                  (else
                    (c:code 
                      (string-append
                        (c:allocs->str (c:allocs cfun) "\n")
                        (c:allocs->str (c:allocs cargs) "\n")
                        "return_closcall" (number->string (c:num-args cargs))
                        "(data,"
                        this-cont
                        (if (> (c:num-args cargs) 0) "," "")
                        (c:body cargs)
                        ");")))))))))

        ((tagged-list? '%closure fun)
         (let* ((cfun (c-compile-closure 
                        fun append-preamble cont ast-id trace cps?))
                (this-cont (string-append "(closure)" (c:body cfun)))
                (cargs (c-compile-args
                         args append-preamble "  " this-cont ast-id trace cps?))
                (num-cargs (c:num-args cargs)))
           (cond
             ((not cps?)
              (c:code
                (string-append
                   (c:allocs->str (c:allocs cfun) "\n")
                   (c:allocs->str (c:allocs cargs) "\n")
                   "return_copy(ptr,"
                   (c:body cargs)
                   ");")))
             (else ; CPS, IE normal behavior
               (set-c-call-arity! num-cargs)
               ;; TODO: see corresponding code in %closure-ref that outputs return_closcall.
               ;; need to use (well-known-lambda) to check the ref to see if it is a WKL.
               ;; if so, lookup ast and use cgen-id to map back to emit the lambda_gc_ret there
               (with-fnc (ast:lambda-id (closure->lam fun)) (lambda (fnc)
                 (cond
                  ((and *optimize-well-known-lambdas*
                          (adbf:well-known fnc)
                          (equal? (adbf:closure-size fnc) 1))
                   (let* ((lid (ast:lambda-id (closure->lam fun)))
                          (c-lambda-fnc-str (string-append "__lambda_" (number->string lid)))
                          (c-lambda-fnc-gc-ret-str (string-append "__lambda_gc_ret_" (number->string lid))))
                     (c:code
                       (string-append
                          (c:allocs->str (c:allocs cfun) "\n")
                          (c:allocs->str (c:allocs cargs) "\n")
                          "return_direct_with_obj" (number->string num-cargs)
                          "(data,"
                          this-cont
                          ","
                          c-lambda-fnc-gc-ret-str
                          ","
                          c-lambda-fnc-str
                          (if (> num-cargs 0) "," "")
                          (c:body cargs)
                          ");"))))
                  ((adbf:well-known fnc)
                   (let* ((lid (ast:lambda-id (closure->lam fun)))
                          (c-lambda-fnc-str (string-append "__lambda_" (number->string lid))))
                     (c:code
                       (string-append
                          (c:allocs->str (c:allocs cfun) "\n")
                          (c:allocs->str (c:allocs cargs) "\n")
                          "return_direct_with_clo" (number->string num-cargs)
                          "(data,"
                          this-cont
                          ","
                          c-lambda-fnc-str
                          (if (> num-cargs 0) "," "")
                          (c:body cargs)
                          ");"))))
                  (else
                   (c:code
                     (string-append
                        (c:allocs->str (c:allocs cfun) "\n")
                        (c:allocs->str (c:allocs cargs) "\n")
                        "return_closcall" (number->string num-cargs)
                        "(data,"
                        this-cont
                        (if (> num-cargs 0) "," "")
                        (c:body cargs)
                        ");"))))))))))

        ((equal? 'Cyc-seq fun)
         (let ((exps (foldr
                       (lambda (expr acc)
                         ;; Join expressions; based on c:append
                         (let ((cp1 (if (ref? expr)
                                        ;; Ignore lone ref to avoid C warning
                                        (c:code/vars "" '())
                                        (c-compile-exp expr append-preamble cont ast-id trace cps?)))
                               (cp2 acc))
                           (c:code/vars 
                             (let ((cp1-body (c:body cp1)))
                               (if (zero? (string-length cp1-body))
                                   (c:body cp2) ; Ignore cp1 if necessary
                                   (string-append cp1-body ";" (c:body cp2))))
                             (append (c:allocs cp1) (c:allocs cp2)))))
                       (c:code "")
                       args)))
          exps))
        ((equal? 'Cyc-local-set! fun)
         ;:(trace:error `(JAE DEBUG Cyc-local-set ,exp))
         (let ((val-exp (c-compile-exp (caddr exp) append-preamble cont ast-id trace cps?)))
           (c:code/vars
             (string-append (mangle (cadr exp)) " = " (c:body val-exp) ";")
             (c:allocs val-exp)))
         ;; (c:code (string-append (mangle (cadr exp)) " = " (mangle (caddr exp)) ";"))
        )
        ((equal? 'let fun)
         (let* ((vars/vals (cadr exp))
                (body (caddr exp))
                (vexps (foldr
                        (lambda (var/val acc)
                          (set-use-alloca! #t) ;; Force alloca to ensure safe c stack allocs
                          ;; Join expressions; based on c:append
                          (let ((cp1 (c-compile-exp (cadr var/val) append-preamble cont ast-id trace cps?))
                                (cp2 acc))
                            (set-use-alloca! #f) ; Revert flag
                            (c:code/vars 
                              (let ((cp1-body (c:body cp1)))
                                (string-append cp1-body ";" (c:body cp2)))
                              (append 
                                (list (string-append "object " (mangle (car var/val)) ";"))
                                (c:allocs cp1) 
                                (c:allocs cp2)))))
                        (c:code "")
                        vars/vals))
               (body-exp (c-compile-exp 
                           body append-preamble cont ast-id trace cps?)))
           ;;(trace:error `(JAE DEBUG body ,body ,vars/vals ,exp))
          (c:append vexps body-exp)))
        (else
         (error `(Unsupported function application ,exp)))))))

;; c-compile-if : if-exp -> string
(define (c-compile-if exp append-preamble cont ast-id trace cps?)
  (let* ((compile (lambda (exp)
                    (c-compile-exp exp append-preamble cont ast-id trace cps?)))
         (test (compile (if->condition exp)))
         (then (compile (if->then exp)))
         (els (compile (if->else exp))))
  (c:code (string-append
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
;; (define *global-inline-scms* '())
;; (define (add-global-inline-scm-lambda var-sym code)
;;  (add-global-inline var-sym )
;;  (set! *global-inline-scms*
;;        (cons (list var-sym code) *global-inline-scms*)))

;; Global compilation
(define *globals* '())
(define *global-syms* '())
(define (global-lambda? global) (cadr global))
(define (global-not-lambda? global) (not (cadr global)))
(define (add-global var-sym lambda? code)
  ;; (write `(add-global ,var-sym ,code))
  (set! *globals* (cons (list var-sym lambda? code) *globals*)))
(define (c-compile-global exp append-preamble cont trace)
 (let ((var (define->var exp))
       (body (if (equal? 4 (length exp)) ; Simple var assignment contains superfluous %closure-ref
                 (cadddr exp)
                 (car (define->exp exp)))))
   (add-global 
     var 
     (ast:lambda? body) 
     (c-compile-exp 
      body append-preamble cont
      (if (ast:lambda? body)
          (ast:lambda-id body)
          -1)
      (st:add-function! trace var) #t))

   ;; Add inline global definition also, if applicable
   ;;   (trace:error `(JAE DEBUG ,var
   ;;           ,(lambda? body)
   ;;           ,(define-c->inline-var exp)
   ;;           ,(prim:udf? (define-c->inline-var exp))
   ;;           ))
   (when (and (ast:lambda? body)
              (prim:udf? (define-c->inline-var exp)))
       (add-global-inline 
         var
         (define-c->inline-var exp))
       (add-global 
         (define-c->inline-var exp)
         #t ; always a lambda
         (c-compile-exp 
          body append-preamble cont
          (ast:lambda-id body)
          (st:add-function! trace var)
          #f ; inline, so disable CPS on this pass
         )))

   (c:code/vars "" (list ""))))

(define (c-compile-raw-global-lambda exp append-preamble cont trace . cps?)
   (let* ((precompiled-sym
            (if (equal? cps? '(#f))
                'precompiled-inline-lambda
                'precompiled-lambda))
          (lambda-data
            `(,precompiled-sym
              ,(caddr exp) ; Args
              ,(cadddr exp))) ; Body
          (lid (allocate-lambda #f lambda-data))
          (total-num-args
            (let ((count 1)) ; Start at 1 because there will be one less comma than args
              (string-for-each 
                (lambda (c) 
                  (if (equal? #\, c) (set! count (+ count 1))))
                (caddr exp))
              count)) ; args
          ;; Subtract "internal" args added for runtime
          (num-args
            (- total-num-args 4)))
     ;; Is the function also defined inline?
     ;; (trace:error `(JAE define-c ,exp))
     (cond
      ((> (length exp) 4)
       ;; (trace:error `(JAE define-c inline detected))
       (let ((fnc-sym 
               (define-c->inline-var exp)))
         ;; (trace:error `(JAE define-c inline detected ,fnc-sym))
         (add-global-inline (define->var exp) fnc-sym)
         (c-compile-raw-global-lambda 
           `(define-c ,fnc-sym ,@(cddddr exp))
           append-preamble 
           cont 
           trace
           #f)))) ; Inline this one; CPS will not be used
     ;; Add this define-c
     (add-global 
       (define->var exp)
       #t ; (lambda? body) 
       (let ((cv-name (mangle (gensym 'c))))
         (c:code/vars 
           (string-append "&" cv-name)
           (list
             (string-append "mclosure0(" cv-name ", (function_type)__lambda_"
               (number->string lid) ");" cv-name ".num_args = "
               (number->string num-args)
               ";")))))
     (c:code/vars "" (list ""))))

;; Symbol compilation

(define *symbols* '())

(define (allocate-symbol sym)
  (if (not (member sym *symbols*))
      ;; (not (Cyc-reserved-symbol? sym)))
      (set! *symbols* (cons sym *symbols*))))

;; Lambda compilation.

;; Lambdas get compiled into procedures that,
;; once given a C name, produce a C function
;; definition with that name.

;; These procedures are stored up and eventually
;; emitted.

;; type lambda-id = natural

;; num-lambdas : natural
(define num-lambdas 0)

;; lambdas : alist[lambda-id,string -> string]
(define lambdas '())
(define inline-lambdas '())

;; allocate-lambda : (Either ast:lambda boolean) -> (string -> string) -> integer
;; Create/store/return a unique lambda-id for the given function.
(define (allocate-lambda ast:lam lam . cps?)
  (let ((id num-lambdas))
    (cond
      ((and ast:lam (not (equal? cps? '(#f))))
        (set! id (ast:lambda-id ast:lam)))
      (else
        (set! num-lambdas (+ 1 num-lambdas))))
    (set! lambdas (cons (list id lam ast:lam) lambdas))
    (if (equal? cps? '(#f))
        (set! inline-lambdas (cons id inline-lambdas)))
    ;; (when ast:lam
    ;;  (with-fnc! (ast:lambda-id ast:lam) (lambda (fnc)
    ;;    (adbf:set-cgen-id! fnc id))))
    id))

;; get-lambda : lambda-id -> (symbol -> string)
;; (define (get-lambda id)
;;  (cdr (assv id lambdas)))

(define (lambda->env exp)
    (let ((formals (ast:lambda-formals->list exp)))
        (if (pair? formals)
            (car formals)
            'unused)))

;;           (tmp-ident (if (> (length (lambda-formals->list exp)) 0)
;;                          (mangle (if (pair? (lambda->formals exp))
;;                                      (car (lambda->formals exp))
;;                                      (lambda->formals exp)))
;;                          ""))
;;           (has-closure?
;;             (and
;;               (> (string-length tmp-ident) 3)
;;               (equal? "self" (substring tmp-ident 0 4))))

;; Compute the minimum number of arguments a function expects.
;; Note this must be the count before additional closure/CPS arguments
;; are added, so we need to detect those and not include them.
(define (compute-num-args lam)
  (let ((count (ast:lambda-num-args lam))) ; Current arg count, may be too high
    (cond
     ((< count 0) -1) ; Unlimited
      (else
        (let ((formals (ast:lambda-formals->list lam)))
          (- count
             (if (fl/closure? formals) 1 0)
             (if (fl/cont? formals) 1 0)))))))

;; Minimum number of required arguments for a lambda
(define (ast:lambda-num-args exp)
  (let ((type (ast:lambda-formals-type exp))
        (num (length (ast:lambda-formals->list exp))))
    (cond
      ((equal? type 'args:varargs)
       -1) ; Unlimited
      ((equal? type 'args:fixed-with-varargs)
       (- num 1)) ; Last arg is optional
      (else
        num))))

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

;; c-compile-closure-element-ref :: integer -> symbol -> integer -> string
;;
;; Compile a reference to an element of a closure.
(define (c-compile-closure-element-ref ast-id var idx)
  (with-fnc ast-id (lambda (fnc)
    ;; (trace:info `(c-compile-closure-element-ref ,ast-id ,var ,idx ,fnc))
    (cond
      ((and *optimize-well-known-lambdas*
            (adbf:well-known fnc)
            ;; (pair? (adbf:all-params fnc))
            (equal? (adbf:closure-size fnc) 1))
       (mangle (car (adbf:all-params fnc))))
      (else
        (string-append 
          "((closureN)" (mangle var) ")->elements[" idx "]"))))))

;; Analyze closure members and assign index of the function's "self" closure, if found
;; Parameters:
;;   ast-fnc - Function to check for, in AST lambda form
;;   closure-args - Members of the closure to scan
(define (find-closure-assigned-var-index! ast-fnc closure-args)
  (let ((index 0)
        (fnc (adb:get/default (ast:lambda-id ast-fnc) #f)))
    ;; (trace:info `(find-closure-assigned-var-index! ,ast-fnc ,fnc ,closure-args))
    (cond
      ((and fnc 
            (pair? (adbf:assigned-to-var fnc)))
       (for-each
        (lambda (arg)
          (when (and (ref? arg) (member arg (adbf:assigned-to-var fnc)))
            ;; (trace:error `(JAE closure for ,(ast:lambda-id ast-fnc) self ref is index ,index))
            (adbf:set-self-closure-index! fnc index)
            (adb:set! (ast:lambda-id ast-fnc) fnc))
          (set! index (+ index 1)))
        closure-args))
      (else #f))))

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
(define (c-compile-closure exp append-preamble cont ast-id trace cps?)
  (find-closure-assigned-var-index! (closure->lam exp) (cdr exp))
  (let* ((lam (closure->lam exp))
         (use-alloca? (alloca? ast-id trace))
         (free-vars
           (map
             (lambda (free-var)
                (if (tagged-list? '%closure-ref free-var)
                    (let ((var (cadr free-var))
                          (idx (number->string (- (caddr free-var) 1))))
                        (c-compile-closure-element-ref ast-id var idx)
                        ;; (string-append
                        ;;    "((closureN)" (mangle var) ")->elements[" idx "]")
                    )
                    (mangle free-var)))
             (closure->fv exp))) ; Note these are not necessarily symbols, but in cc form
         (cv-name (mangle (gensym 'c)))
         (lid (allocate-lambda lam (c-compile-lambda lam trace cps?) cps?))
         (use-obj-instead-of-closure? 
           (with-fnc (ast:lambda-id lam) (lambda (fnc)
             (and *optimize-well-known-lambdas*
                  (adbf:well-known fnc) ; Only optimize well-known functions
                  ;; (equal? (length free-vars) 1) ; Sanity check
                  (equal? (adbf:closure-size fnc) 1))))) ; From closure conv
         (macro? (assoc (st:->var trace) (get-macros)))
         (call/cc? (and (equal? (car trace) "scheme/base.sld")
                        (equal? (st:->var trace) 'call/cc)))
         (num-args-str 
          (if call/cc?
            "1" ; Special case, need to change runtime checks for call/cc
            (number->string (compute-num-args lam))))
         (create-object (lambda ()
           ;; JAE - this is fine, now need to handle other side (actually reading the value without a closure obj
           ;; (trace:error `(create-object free-vars ,free-vars ,(car free-vars)))
           (c:code/vars
             (car free-vars)
             (list))))
         (create-nclosure (lambda ()
           (let* ((decl (if use-alloca?
                            (string-append "closureN_type * " cv-name " = alloca(sizeof(closureN_type));\n")
                            (string-append "closureN_type " cv-name ";\n")))
                  (ev-name (mangle (gensym 'e)))
                  (elem-decl 
                    (if use-alloca?
                        (string-append "object *" ev-name " = (object *)alloca(sizeof(object) * " 
                                                     (number->string (length free-vars)) ");")
                        (string-append "object " ev-name " [" (number->string (length free-vars)) "];\n")))
                  (sep (if use-alloca? "->" ".")))
             (string-append
               decl
               elem-decl
               ;; Not ideal, but one more special case to type check call/cc
               (if call/cc?  "Cyc_check_proc(data, f);\n" "")
               cv-name sep "hdr.mark = gc_color_red;\n "
               cv-name sep "hdr.grayed = 0;\n"
               cv-name sep "tag = closureN_tag;\n "
               cv-name sep "fn = (function_type)__lambda_" (number->string lid) ";\n"
               cv-name sep "num_args = " num-args-str ";\n"
               cv-name sep "num_elements = " (number->string (length free-vars)) ";\n"
               cv-name sep "elements = (object *)" ev-name ";\n";
               (let loop ((i 0) 
                          (vars free-vars))
                 (if  (null? vars)
                   ""
                   (string-append 
                     cv-name sep "elements[" (number->string i) "] = " 
                             (car vars) ";\n"
                     (loop (+ i 1) (cdr vars)))))))))
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
              cv-name ".num_args = " (number->string (compute-num-args lam)) ";")))))
  ;; (trace:info (list 'JAE-DEBUG trace macro?))
  (cond
    (use-obj-instead-of-closure?
      (create-object))
    (else
      (c:code/vars
        (if (and use-alloca?
                 (> (length free-vars) 0))
            cv-name
            (string-append "&" cv-name))
        (list 
          (if (> (length free-vars) 0)
            (create-nclosure)
            (create-mclosure))))))))

;; c-compile-formals : list[symbol] -> string
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

;; c-compile-lambda : lamda-exp (string -> void) -> (string -> string)
(define (c-compile-lambda exp trace cps?)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n")))))
    (let* ((formals (c-compile-formals 
                      (if (not cps?)
                          ;; Ignore continuation (k) arg for non-CPS funcs
                          (cdr (ast:lambda-args exp))
                          (ast:lambda-args exp))
                      (ast:lambda-formals-type exp)))
           (tmp-ident (if (> (length (ast:lambda-formals->list exp)) 0) 
                          (mangle (if (pair? (ast:lambda-args exp))
                                      (car (ast:lambda-args exp))
                                      (ast:lambda-args exp)))
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
             (or
               (adbf:calls-self? (adb:get/default (ast:lambda-id exp) (adb:make-fnc)))
               ;; Older direct recursive logic
               (and (not has-closure?) ; Only top-level functions for now
                    (pair? trace)
                    (not (null? (cdr trace)))
                    (adbv:direct-rec-call? (adb:get (cdr trace))))))
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
                        (car (ast:lambda-body exp)) ; car ==> assume single expr in lambda body after CPS
                        append-preamble
                        (mangle env-closure)
                        (ast:lambda-id exp)
                        trace 
                        cps?)))
     (cons 
      (lambda (name)
        (string-append "static " return-type " " name 
                       "(void *data, " arg-argc
                        formals*
                       ") {\n"
                       preamble
                       (if (ast:lambda-varargs? exp)
                         ;; Load varargs from C stack into Scheme list
                         (string-append 
                          ;; DEBUGGING:
                          ;; "printf(\"%d %d\\n\", argc, "
                          ;;  (number->string (length (ast:lambda-formals->list exp))) ");"
                           "load_varargs(" 
                           (mangle (ast:lambda-varargs-var exp))
                           ", "
                           (mangle (ast:lambda-varargs-var exp))
                           "_raw, argc - " (number->string 
                                         (- (length (ast:lambda-formals->list exp)) 
                                            1
                                            (if has-closure? 1 0)))
                           ");\n");
                         "") ; No varargs, skip
                       (c:serialize
                         (c:append
                           (c:code 
                            ;; Only trace when entering initial defined function
                             (cond
                               (has-closure?
                                (if has-loop? "\n while(1) {\n" ""))
                               (else
                                 (string-append
                                   (st:->code trace)
                                   (if has-loop? "\n while(1) {\n" "")))))
                           body)
                         "  ")
                       "; \n"
                       (if has-loop? "}\n" "")
                       "}\n"))
      formals*))))
  
(define cgen:mangle-global #f)

(define (ast:lambda-varargs-var exp)
  (if (ast:lambda-varargs? exp)
    (if (equal? (ast:lambda-formals-type exp) 'args:varargs)
        (ast:lambda-args exp) ; take symbol directly
        (car (reverse (ast:lambda-formals->list exp)))) ; Last arg is varargs
    #f))

(define (ast:lambda-varargs? exp)
  (let ((type (ast:lambda-formals-type exp)))
    (or (equal? type 'args:varargs)
        (equal? type 'args:fixed-with-varargs))))

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
                      src-file
                      flag-set?)
  (set! *global-syms* (append globals (lib:idb:ids import-db)))
  (set! *cgen:track-call-history*  (flag-set? 'track-call-history))
  (set! *cgen:use-unsafe-prims*  (flag-set? 'use-unsafe-prims))
  (set! num-lambdas (+ (adb:max-lambda-id) 1))
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
    ;; Print the prototypes:
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
    
    ;; Print GC return wrappers
    (for-each
     (lambda (l)
      (let ((ast (caddr l)))
        (when (ast:lambda? ast)
          (with-fnc (ast:lambda-id ast) (lambda (fnc)
            ;;(when (and 
            ;;           (adbf:well-known fnc)
            ;;           (equal? (adbf:closure-size fnc) 1))
            ;;  (trace:error `(JAE ,(car l) ,l ,fnc)))

            (when (and *optimize-well-known-lambdas*
                       (adbf:well-known fnc)
                       (equal? (adbf:closure-size fnc) 1))
              ;; (trace:error `(JAE ,(car l) ,l ,fnc))
           (let* ((params-str (cdadr l))
                  (args-str
                    (string-join
                      (cdr
                        (string-split
                          (string-replace-all params-str "object" "")
                          #\,))
                      #\,)))
             (emit*
               "static void __lambda_gc_ret_"
               (number->string (car l))
               "(void *data, int argc,"
               params-str
               ")"
               "{"
                  "\nobject obj = "
                  "((closure1)" (mangle (car (adbf:all-params fnc))) ")->element;\n"
                  "__lambda_"
                  (number->string (car l))
                  "(data, argc, obj"
                  (if (> (string-length args-str) 0)
                      (string-append "," args-str))
                  ");"
               "}"))))))))
     lambdas)

    ;; Print the definitions:
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
           " }"))
       ((equal? 'precompiled-inline-lambda (caadr l))
         (emit*
           "static object __lambda_" 
           (number->string (car l))
           (cadadr l)
           " {"
           (car (cddadr l))
           " }"))
       ((member (car l) inline-lambdas)
        (emit ((caadr l) (string-append "__lambda_" (number->string (car l))))))
       (else
         (emit ((caadr l) (string-append "__lambda_" (number->string (car l))))))))
     lambdas)
  
    ;; Emit inlinable function list
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
          (emit* " } "))))

    ;; Emit entry point
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
        ;; DEBUG (emit (string-append "printf(\"init " (lib:name->string lib-name) "\\n\");"))
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
        (let ((mglo (cgen:mangle-global (car global))))
          (emits (string-append
                   "\n  add_global(\""
                   mglo
                   "\", (object *) &"))
          (emits mglo)
          (emits ");")))
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
          (let ( ;; (cvar-sym (mangle (gensym 'cvar)))
                (pair-sym (mangle (gensym 'pair)))
                (clo-sym (mangle (gensym 'clo)))
                (fnc (string-append
                       "c_" (lib:name->string lib-name) "_inlinable_lambdas")))
           (emits* 
            "  mclosure0(" clo-sym ", " fnc "); "
            ;; "  make_cvar(" cvar-sym
            ;; ", (object *)&" fnc ");"
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
             (set! pairs (cons pair-sym pairs))))
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
                  ", &" prev-clo ");"))
            (reverse required-libs)) ;; Init each lib's dependencies 1st
          (emit* 
            ;; Start cont chain, but do not assume closcall1 macro was defined
            "(" this-clo ".fn)(data, 0, &" this-clo ", &" this-clo ");")
          (emit "}")
          (emit "static void c_entry_pt_first_lambda(void *data, int argc, closure cont, object value) {")
          ;; DEBUG (emit (string-append "printf(\"init first lambda\\n\");"))
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
            ;; GC to ensure objects are moved when exporting exports.
            ;; Otherwise there will be broken hearts :(
            (emit*
              "  mclosure1(clo, c_" (lib:name->string lib-name) "_entry_pt_first_lambda, ((closure1_type *)cont)->element);\n"
              "  object buf[1]; buf[0] = cont;\n"
              "  GC(data, (closure)&clo, buf, 1);\n"))))

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
       (pp code fp)) ; CHICKEN pretty-print
      (else
       (write code fp)))))

))
