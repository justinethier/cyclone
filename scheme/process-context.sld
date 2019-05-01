;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the process-context r7rs.
;;;;
(define-library (scheme process-context)
  (export 
    command-line
    ;exit - not needed because already defined as a primitive, at least for now
    emergency-exit
    get-environment-variable
    get-environment-variables
  )
  (begin
    (define emergency-exit exit)
    (define-c command-line
      "(void *data, int argc, closure _, object k)"
      ;; TODO: consolidate with Cyc_command_line_arguments from runtime.c
      " int i;
        object lis = NULL;
        for (i = _cyc_argc; i > 0; i--) {
          object ps = alloca(sizeof(string_type));
          object pl = alloca(sizeof(pair_type));
          make_utf8_string(data, s, _cyc_argv[i - 1]);
          memcpy(ps, &s, sizeof(string_type));
          ((list)pl)->hdr.mark = gc_color_red;
          ((list)pl)->hdr.grayed = 0;
          ((list)pl)->hdr.immutable = 0;
          ((list)pl)->tag = pair_tag;
          ((list)pl)->pair_car = ps;
          ((list)pl)->pair_cdr = lis;
          lis = pl;
        }
        return_closcall1(data, k, lis); ")
    (define-c get-environment-variable
      "(void *data, int argc, closure _, object k, object env_var)"
      ;; TODO: consolidate with Cyc_command_line_arguments from runtime.c
      " 
        const char *v = NULL;
        Cyc_check_str(data, env_var);
        v = getenv(string_str(env_var));
        if (v == NULL) {
          return_closcall1(data, k, boolean_f);
        } else {
          make_utf8_string(data, str, v);
          return_closcall1(data, k, &str);
        }
      ")
    (define-c get-environment-variables
      "(void *data, int argc, closure _, object k)"
      "pack_env_variables(data, k); 
      ")
  ))
