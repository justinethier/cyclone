(define-library (scheme time)
  (export 
    current-second
    current-jiffy
    jiffies-per-second
  )
  (import (scheme base) 
  )
  (begin
    ;; Experimenting with what an FFI could look like
    ;; TODO: also need a way to add #include's, and later on compiler options (may already have that, need to check)
    ;;
      ;; want the signature to be similar to this:
      ;; static void __lambda_0(void *data, int argc, closure _,object k_7322, object arg1_737, object arg2_736) {
      ;; lambda portion is computed, so we can't include that.
      ;; compiler would need to insert the "static void (lambda)" part
;; TODO: maybe break up into two args, one being the args list and the other being the function body??
    (define-c current-jiffy
      "(void *data, int argc, closure _, object k)"
      ;; TODO: actually get the current jiffy
      " make_int(temp, 0);
        return_closcall1(data,  k, &temp); ")
    (define jiffies-per-second 0) ;; TODO: just a placeholder at the moment
    (define current-second 0) ;; TODO: just a placeholder at the moment
  ))
