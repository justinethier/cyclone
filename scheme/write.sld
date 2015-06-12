(define-library (scheme write)
  (export
    display
    write
  )
  (import (scheme base))
  (begin
    (define (display obj . port)
      (if (null? port)
          (Cyc-display obj (current-output-port))
          (Cyc-display obj (car port))))
    (define (write obj . port)
      (if (null? port)
          (Cyc-write obj (current-output-port))
          (Cyc-write obj (car port))))
  )
)
