(define-library (scheme cyclone common)
  (export 
    *Cyc-version-banner*
    *version*
    *version-banner*
    *c-file-header-comment* 
    *features*)
  (include "common.scm")
  (begin
    (define *Cyc-version-banner* *version-banner*)))
