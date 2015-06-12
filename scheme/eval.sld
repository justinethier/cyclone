(define-library (scheme eval)
  (import 
    (scheme base)
    (scheme write)
    (scheme read))
  (export
    eval
  )
  (include "../eval.scm")
  (begin))
