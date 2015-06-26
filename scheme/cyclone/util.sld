(define-library (scheme cyclone util)
  (import (scheme base)
          (scheme char))
  (export
    filter
    tagged-list?
    mangle
    mangle-global)
  (include "../../util.scm"))
