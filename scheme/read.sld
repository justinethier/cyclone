(define-library (scheme read)
  (import (scheme base))
  (export
    ;read  ;; what about issues with read/cyc-read in csi???
    cyc-read ;; try this first, but it needs to go away!!!
    read-all
  )
  (include "../parser.scm")
  (begin))
