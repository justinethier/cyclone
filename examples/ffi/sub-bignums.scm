;; A basic example of subtracting 2 bignums using the FFI.
;;
;; This example also shows how to pass the current thread's 
;; data object to C using the thread-data type specifier.
;; The thread data object is often needed so it can be passed 
;; along to functions in the Cyclone runtime.
;;
(import (scheme base) (scheme write) (cyclone foreign))
(include-c-header "sub-bignums.h")

;; Define a C function receiving thread data and two bignum arguments
;; Note thread data is passed implicitly, so calls to sub-big-num do
;; not need to pass the thread data argument from scheme code.
(c-define sub-big-nums bignum "sub_big_nums" thread-data bignum bignum)

(display
  (sub-big-nums 
     999999999999999999999999
     222222222222222222222222))
(newline)
