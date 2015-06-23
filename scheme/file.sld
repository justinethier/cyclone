(define-library (scheme file)
  (export
    call-with-input-file
    call-with-output-file
    with-input-from-file
    with-output-from-file
  )
  (import (scheme base))
  (begin
    (define (call-with-input-file string proc)
      (call-with-port (open-input-file string) proc))
    (define (call-with-output-file string proc)
      (call-with-port (open-output-file string) proc))
    TODO: with-input-from-file
    TODO: with-output-from-file
))
