(define-library (scheme cyclone iset constructors)
  (export
   iset iset-copy list->iset list->iset! iset-map
   iset-adjoin iset-adjoin! iset-delete iset-delete!
   iset-union iset-union! iset-intersection iset-intersection!
   iset-difference iset-difference!
   ;; low-level
   iset-copy-node iset-squash-bits! iset-insert-left! iset-insert-right!)
  (import (scheme base)
          (scheme cyclone iset base)
          (scheme cyclone iset iterators)
          (srfi 60))
  (include "constructors.scm"))
  
