(define-library (scheme process-context)
  (export 
    command-line
    ;exit - already defined as a primitive, at least for now
    emergency-exit
    ; TODO: get-environment-variable
    ; TODO: get-environment-variables
  )
;  (import (scheme base) 
;  )
  (begin
    (define emergency-exit exit)
    (define-c command-line
      "(void *data, int argc, closure _, object k)"
      ;; TODO: consolidate with Cyc_command_line_arguments from runtime.c
      " int i;
        object lis = nil;
        for (i = _cyc_argc; i > 0; i--) {
          object ps = alloca(sizeof(string_type));
          object pl = alloca(sizeof(cons_type));
          make_string(s, _cyc_argv[i - 1]);
          memcpy(ps, &s, sizeof(string_type));
          ((list)pl)->hdr.mark = gc_color_red;
          ((list)pl)->hdr.grayed = 0;
          ((list)pl)->tag = cons_tag;
          ((list)pl)->cons_car = ps;
          ((list)pl)->cons_cdr = lis;
          lis = pl;
        }
        return_closcall1(data, k, lis); ")
  ))
