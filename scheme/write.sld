(define-library (scheme write)
  (export
    write
  )
  (import (scheme base))
  (begin
    (define (write obj . port)
      (if (null? port)
          (Cyc-write obj (current-output-port))
          (Cyc-write obj (car port))))
  )
)
