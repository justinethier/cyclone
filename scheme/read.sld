(define-library (scheme read)
  (import (scheme base)
          (scheme char))
  (export
    read
    read-all
  )
  (include "parser.scm")
  (begin
    (define read cyc-read)))
