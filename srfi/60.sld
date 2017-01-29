;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2017, Justin Ethier
;;;; Copyright (c) 2017, Koz Ross
;;;;
;;;; This module is an interface to the Integers as Bits library.
;;;;
;;;; NOTE: This does not support arbitrary-precision numbers, as Cyclone 
;;;; doesn't have them yet. This will need to be modified accordingly once 
;;;; this support is provided.
(define-library (srfi 60)
  (import (scheme base)
          (scheme inexact))
  (export
    any-bits-set? arithmetic-shift ash
    bit-count bit-field bit-set? bitwise-and bitwise-if bitwise-ior 
      bitwise-merge bitwise-not bitwise-xor booleans->integer
    copy-bit copy-bit-field 
    first-set-bit
    integer-length integer->list
    list->integer log2-binary-factors logbit? logand logcount logior lognot 
      logtest logxor
    reverse-bit-field rotate-bit-field)
  (include "60.scm")
  (begin)
)
