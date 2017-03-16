(define-library (atomics)
  (export
    atomic:get
    atomic:set!
  )
  (include-c-header "<ck_pr.h>")
  (begin
    ;TODO: won't work, ints are immutable
    ;(define-c atomic:fx++
    ;  "(void *data, int argc, closure _, object k, object num)"
    ;  " Cyc_check_fixnum(data, num);
    ;    ck_pr_add_ptr(&num, 2);
    ;    return_closcall1(data, k, num); ")
  ))

