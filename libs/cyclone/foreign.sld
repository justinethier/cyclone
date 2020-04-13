;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2019, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; TBD
;;;;
(define-library (cyclone foreign)
 (import
   (scheme base)
   ;(scheme write) ;; TODO: debugging only!
 )
 ;(include-c-header "<ck_pr.h>")
 (export
  ;; TODO
 )
 (begin
  ;; TODO: internal to compiler? Anything to define in this library??
  ;; internal name could be different (Cyc-foreign-code) to facilitate
  ;; library renaming, etc here
  ;(foreign-code STRING ...)
 )
)
