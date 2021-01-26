;; A basic example of subtracting 2 bignums using the FFI.
;;
;; This example is notable because we need to pass the
;; current thread's data object to C so that we can pass
;; it along to functions in the Cyclone runtime.
;;
(import (scheme base) (scheme write) (cyclone foreign) (srfi 18))
(include-c-header "sub-bignums.h")

(c-define sub-big-nums bignum "sub_big_nums" opaque bignum bignum)

(display
  (sub-big-nums 
     (current-thread-data)
     999999999999999999999999
     222222222222222222222222))
(newline)
