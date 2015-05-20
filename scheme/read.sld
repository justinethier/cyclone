(define-library (scheme read)
  (import (scheme base))
  (export
    read  ;; what about issues with read/cyc-read in csi???
    read-all
  )
  (include "parser.scm")
  (begin))
