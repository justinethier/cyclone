(define-library (scheme read)
  (export
    read  ;; what about issues with read/cyc-read in csi???
    read-all
  )
  (include "parser.scm")
  (begin))
