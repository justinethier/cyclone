(import (scheme base)
        (scheme file)
        (scheme write))

(call-with-output-file "tmp.c"
  (lambda (fp)
    (display "
#include \"cyclone.h\"
#include \"runtime.h\"

void do_dispatch(int argc, function_type func, object clo, object *b) {
  switch(argc) {" fp)

    (define bs "")
    ;(for-each
    ;  (lambda (i)
    (let loop ((i 0))

        (display "case " fp)
        (display i fp)
        (display ":func(" fp)
        (display i fp)
        (display ",clo" fp)
        (display bs fp)
        (display ");" fp)

        (set! bs (string-append bs ",*(b+" (number->string i) ")"))
        
        (if (< i 129)
          (loop (+ i 1))))
    ;  '(0 1 2))
;  case (0): func(0, clo);
;  case (1): func(1, clo, *(b+0));
;  case (2): func(2, clo, *(b+0), *(b+1));

    (display "
  default:
  {
   char buf[1024];
   snprintf(buf, 1023, \"Unhandled number of function arguments: %d\\n\", argc); 
   Cyc_rt_raise_msg(buf);
  }
  }
}" fp)))

