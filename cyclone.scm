;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains a front-end for the compiler itself.
;;

(cond-expand
 (chicken
   (require-extension extras) ;; pretty-print
   (require-extension chicken-syntax) ;; when
   (load "parser.so")
   (load "trans.so")
   (load "cgen.so"))
; (husk
;   (import (husk pretty-print))
;   ;; TODO: load files
; )
 (else
   (load "parser.scm")
   (load "trans.scm")
   (load "cgen.scm")))

;; Code emission.
  
; c-compile-and-emit : (string -> A) exp -> void
(define (c-compile-and-emit input-program)
  (call/cc 
    (lambda (return)
      (define globals '())
      (emit *c-file-header-comment*) ; Guarantee placement at top of C file
    
      (trace:info "---------------- input program:")
      (trace:info input-program) ;pretty-print
      
      (set! input-program (add-libs input-program))
    
      (set! input-program (expand input-program))
      (trace:info "---------------- after macro expansion:")
      (trace:info input-program) ;pretty-print

      ;; Separate global definitions from the rest of the top-level code
      (set! input-program 
          (isolate-globals input-program))

      ;; Optimize-out unused global variables
      ;; For now, do not do this if eval is used.
      ;; TODO: do not have to be so aggressive, unless (eval (read)) or such
      (if (not (has-global? input-program 'eval))
          (set! input-program 
            (filter-unused-variables input-program)))

      (trace:info "---------------- after processing globals")
      (trace:info input-program) ;pretty-print
    
      ; Note alpha-conversion is overloaded to convert internal defines to 
      ; set!'s below, since all remaining phases operate on set!, not define.
      ;
      ; TODO: consider moving some of this alpha-conv logic below back into trans?
      (set! globals (global-vars input-program))
      (set! input-program 
        (map
          (lambda (expr)
            (alpha-convert expr globals return))
          input-program))
      (trace:info "---------------- after alpha conversion:")
      (trace:info input-program) ;pretty-print
    
      (set! globals (cons 'call/cc globals))
      (set! input-program 
        (cons
          ;; call/cc must be written in CPS form, so it is added here
          ;; TODO: prevents this from being optimized-out
          ;; TODO: will this cause issues if another var is assigned to call/cc?
          '(define call/cc
            (lambda (k f) (f k (lambda (_ result) (k result)))))
           (map 
             (lambda (expr)
               (cps-convert expr))
             input-program)))
      (trace:info "---------------- after CPS:")
      (trace:info input-program) ;pretty-print

      
      ;; TODO: do not run this if eval is in play, or (better) only do opts that are safe in that case (will be much more limited)
      ;; because of this, programs such as icyc can only be so optimized. it would be much more beneficial if modules like
      ;; eval.scm could be compiled separately and then linked to by a program such as icyc.scm. that would save a *lot* of compile
      ;; time. in fact, it might be more beneficial than adding these optimizations.
      ;;
      ;; TODO: run CPS optimization (not all of these phases may apply)
      ;; phase 1 - constant folding, function-argument expansion, beta-contraction of functions called once,
      ;;           and other "contractions". some of this is already done in previous phases. we will leave
      ;;           that alone for now
;      (set! input-program (cps-opt:contractions input-program))
      ;; phase 2 - beta expansion
      ;; phase 3 - eta reduction
      ;; phase 4 - hoisting
      ;; phase 5 - common subexpression elimination
      ;; TODO: re-run phases again until program is stable (less than n opts made, more than r rounds performed, etc)
      ;; END CPS optimization

    
      (set! input-program
        (map
          (lambda (expr)
            (clear-mutables)
            (analyze-mutable-variables expr)
            (wrap-mutables expr globals))
          input-program))
      (trace:info "---------------- after wrap-mutables:")
      (trace:info input-program) ;pretty-print
    
      (set! input-program 
        (map
          (lambda (expr)
            (if (define? expr)
              ;; Global
             `(define ,(define->var expr)
                ,@(caddr (closure-convert (define->exp expr) globals)))
              (caddr ;; Strip off superfluous lambda
                (closure-convert expr globals))))
          input-program))
    ;    (caddr ;; Strip off superfluous lambda
    ;      (closure-convert input-program)))
      (trace:info "---------------- after closure-convert:")
      (trace:info input-program) ;pretty-print
      
      (if (not *do-code-gen*)
        (begin
          (trace:error "DEBUG, existing program")
          (exit)))
    
      (trace:info "---------------- C code:")
      (mta:code-gen input-program globals)
      (return '())))) ;; No codes to return

;; TODO: longer-term, will be used to find where cyclone's data is installed
(define (get-data-path)
  ".")

(define (get-lib filename)
  (string-append (get-data-path) "/" filename))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (port)
      (read-all port))))

;; Compile and emit:
(define (run-compiler args cc?)
  (let* ((in-file (car args))
         (in-prog (read-file in-file))
         (exec-file (basename in-file))
         (src-file (string-append exec-file ".c"))
         (create-c-file 
           (lambda (program) 
             (with-output-to-file 
               src-file
               (lambda ()
                 (c-compile-and-emit program)))))
         (result (create-c-file in-prog)))
    ;; Load other modules if necessary
    (cond
     ((not (null? result))
      (let ((program
              (append
                (if (member 'eval result) 
                    (read-file (get-lib "eval.scm")) 
                   '())
                (if (member 'read result) 
                    (append
                        (read-file (get-lib "parser.scm"))
                       '((define read cyc-read)))
                   '())
                in-prog)))
        (create-c-file program))))

    ;; Compile the generated C file
    (if cc?
      (system 
        ;; -I is a hack, real answer is to use 'make install' to place .h file
        (string-append "gcc " src-file " -L. -lcyclone -lm -I. -g -o " exec-file)))))

;; Handle command line arguments
(let* ((args (command-line-arguments)) ;; TODO: port (command-line-arguments) to husk??
       (non-opts (filter
                   (lambda (arg) 
                     (not (and (> (string-length arg) 1)
                               (equal? #\- (string-ref arg 0)))))
                   args))
       (compile? #t))
  (if (member "-t" args)
      (set! *trace-level* 4)) ;; Show all trace output
  (if (member "-d" args)
     (set! compile? #f)) ;; Debug, do not run GCC
  (cond
    ((< (length args) 1)
     (display "cyclone: no input file")
     (newline))
    ((or (member "-h" args)
         (member "--help" args))
     (write " -t              Show intermediate trace output in generated C files")
     (write " -d              Only generate intermediate C files, do not compile them")
     (write " -h, --help      Display usage information")
     (write " -v              Display version information")
     (write " --autogen       Cyclone developer use only, create autogen.out file")
     (newline))
    ((member "-v" args)
     (display *version-banner*))
    ((member "--autogen" args)
     (autogen "autogen.out")
     (newline))
    ((member "-v" args)
     (display *version-banner*))
    ((member "--autogen" args)
     (autogen "autogen.out"))
    (else
      (run-compiler non-opts compile?))))

