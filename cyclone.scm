;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains a front-end for the compiler itself.
;;

;; TODO: will need to read these from env somehow.
;; for now they are just hard-coded, but that won't work for an install
(define (cyc:get-lib-dir) "/home/justin/Documents/cyclone/")
(define (cyc:get-clib-dir) "/home/justin/Documents/cyclone/")
(define (cyc:get-include-dir) "/home/justin/Documents/cyclone/")

(cond-expand
 (chicken
   (require-extension extras) ;; pretty-print
   (require-extension chicken-syntax) ;; when
   (load (string-append (cyc:get-lib-dir) "parser.so"))
   (load (string-append (cyc:get-lib-dir) "trans.so"))
   (load (string-append (cyc:get-lib-dir) "cgen.so")))
; (husk
;   (import (husk pretty-print))
;   ;; TODO: load files
; )
 (else
   (load (string-append (cyc:get-lib-dir) "parser.scm"))
   (load (string-append (cyc:get-lib-dir) "trans.scm"))
   (load (string-append (cyc:get-lib-dir) "cgen.scm"))))

;; Code emission.
  
; c-compile-and-emit : (string -> A) exp -> void
(define (c-compile-and-emit input-program)
  (call/cc 
    (lambda (return)
      (define globals '())
      (define module-globals '()) ;; Globals defined by this module
      (define program? #t) ;; Are we building a program or a library?
      (define imports '())
      (define imported-vars '())
      (define lib-name '())
      (define lib-exports '())

      (emit *c-file-header-comment*) ; Guarantee placement at top of C file
    
      (trace:info "---------------- input program:")
      (trace:info input-program) ;pretty-print

      (cond
        ((library? (car input-program))
         (let ((includes (lib:includes (car input-program))))
           (set! program? #f)
           (set! lib-name (lib:name (car input-program)))
           (set! lib-exports (lib:exports (car input-program)))
           (set! imports (lib:imports (car input-program)))
           (set! input-program (lib:body (car input-program)))
           ;; Prepend any included files into the begin section
           (if (not (null? includes))
             (for-each
               (lambda (include)
                 (set! input-program 
                       (append (read-file (string-append 
                                            (lib:import->path lib-name) 
                                            include)) 
                               input-program)))
               includes))))
        ((tagged-list? 'import (car input-program))
         (set! imports (cdar input-program))
         (set! input-program (cdr input-program))
         ;(error (list 'imports (cdar input-program)))
        ))

      ;; Process library imports
      ;; TODO: this may not be good enough, may need to tag library
      ;; imports uniquely to reduce issues when the same variable
      ;; is used by multiple libraries, and to allow renaming of imports.
      ;; As of now, that will have to be dealt with later.
      (trace:info "imports:")
      (trace:info imports)
      (set! imported-vars (lib:resolve-imports imports))
      (trace:info "resolved imports:")
      (trace:info imported-vars)

      ;; TODO: how to handle stdlib when compiling a library??
      ;; either need to keep track of what was actually used,
      ;; or just assume all imports were used and include them
      ;; in final compiled program
      ;(set! input-program (add-libs input-program))
    
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
            (filter-unused-variables input-program lib-exports)))

      (trace:info "---------------- after processing globals")
      (trace:info input-program) ;pretty-print
    
      ; Note alpha-conversion is overloaded to convert internal defines to 
      ; set!'s below, since all remaining phases operate on set!, not define.
      ;
      ; TODO: consider moving some of this alpha-conv logic below back into trans?
      (set! module-globals (global-vars input-program))
      (set! globals (append imported-vars module-globals))
      (set! input-program 
        (map
          (lambda (expr)
            (alpha-convert expr globals return))
          input-program))
      (trace:info "---------------- after alpha conversion:")
      (trace:info input-program) ;pretty-print
    
      (let ((cps (map 
                   (lambda (expr)
                     (cps-convert expr))
                   input-program)))
        (cond
         ((and library? (equal? lib-name '(scheme base)))
           (set! globals (cons 'call/cc globals))
           (set! module-globals (cons 'call/cc module-globals))
           (set! input-program 
             (cons
               ;; call/cc must be written in CPS form, so it is added here
               ;; TODO: prevents this from being optimized-out
               ;; TODO: will this cause issues if another var is assigned to call/cc?
               '(define call/cc
                 (lambda (k f) (f k (lambda (_ result) (k result)))))
                cps)))
         (else
           ;; No need for call/cc yet
           (set! input-program cps))))
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
      (mta:code-gen input-program 
                    program? 
                    lib-name 
                    lib-exports 
                    imported-vars
                    module-globals) 
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
         (program? (not (library? (car in-prog))))
         (lib-deps 
           (if (and program? 
                   (tagged-list? 'import (car in-prog)))
             (delete-duplicates (lib:get-all-import-deps (cdar in-prog)))
            '()))
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
     ((and program?
           (not (null? result)))
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
        (create-c-file program)))) ;; TODO: no, don't do same work twice. real answer is linking

    ;; Compile the generated C file
    ;; TODO: -I is a hack, real answer is to use 'make install' to place .h file
    (if cc?
      (cond
        (program?
          (let ((objs-str 
                  (apply
                    string-append
                    (map
                      (lambda (i)
                        (string-append " " (lib:import->filename i ".o") " "))
                      lib-deps))))
            ;(write `(DEBUG all imports ,lib-deps objs ,objs-str))
            ;(write `(DEBUG ,(lib:get-all-import-deps (cdar in-prog))))
            (if (equal? 0 
                  (system
                    (string-append "gcc " src-file " -I" (cyc:get-include-dir) " -g -c -o " exec-file ".o")))
              (system 
                (string-append "gcc " exec-file ".o " objs-str " -L" (cyc:get-clib-dir) " -lcyclone -lm -I" (cyc:get-include-dir) " -g -o " exec-file)))))
        (else
          (system
            (string-append "gcc " src-file " -I" (cyc:get-include-dir) " -g -c -o " exec-file ".o")))))))
          

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
     (display "
 -t              Show intermediate trace output in generated C files
 -d              Only generate intermediate C files, do not compile them
 -h, --help      Display usage information
 -v              Display version information
 --autogen       Cyclone developer use only, create autogen.out file
")
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

