(define-library (scheme cyclone libraries)
  (import (scheme base)
          (scheme read)
          ; TODO: what else? definitely need trans.scm
  )
  (export
    library?
    lib:name
    lib:name->string
    lib:name->symbol
    lib:result
    lib:exports
    lib:imports
    lib:body
    lib:includes
    lib:import->filename
    lib:import->path
    lib:read-imports
    lib:import->export-list
    lib:resolve-imports
    lib:get-all-import-deps
    lib:get-dep-list
  )
  (include "../../libraries.scm")
  ;(begin
  ;  (define read cyc-read))
)
