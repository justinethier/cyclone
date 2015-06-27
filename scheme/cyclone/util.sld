(define-library (scheme cyclone util)
  (import (scheme base)
          (scheme char))
  ; TODO: really need export-all for these cyclone libs!!
  (export
    filter
    tagged-list?
    mangle
    mangle-global)
  (include "../../util.scm"))
