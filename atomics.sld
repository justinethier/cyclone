(define-library (atomics)
  (export
    atomic:get-fx
    atomic:fx++
  )
  (include-c-header "<ck_pr.h>")
  (begin
    ;TODO: needed, unfortunately (define tmp 1)
    (define-c atomic:fx++
      "(void *data, int argc, closure _, object k, object num)"
      " Cyc_check_fixnum(data, num);
        ck_pr_add_ptr(&num, 2);
        return_closcall1(data, k, num); ")
  ))

