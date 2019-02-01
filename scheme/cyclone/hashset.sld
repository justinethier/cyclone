
;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2018, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains a hashset based on code from:
;;;; https://github.com/avsej/hashset.c
;;;;
;;;; NOTE this module only checks for shallow equality (IE: eq?) which
;;;; makes it suitable to test only certain types such as symbols,
;;;; numbers, etc. Hashset should NOT be used to store strings, lists,
;;;; vectors, and objects that require tests for deep equality (IE: equal?).
;;;;
(define-library (scheme cyclone hashset)
    (export
      hs-create
      hs-destroy!
      hs-num-items
      hs-add!
      hs-add-all!
      hs-remove!
      hs-member?
    )
    (import (scheme base)
            (scheme write))
    (include-c-header "cyclone/hashset.h")
    (begin

(define-c hs-create
  "(void *data, int argc, closure _, object k )"
  "
    hashset_t hs = hashset_create();
    make_c_opaque(opq, hs);
    return_closcall1(data, k, &opq);
  ")

(define-c hs-destroy!
  "(void *data, int argc, closure _, object k, object opq )"
  "
    hashset_t hs = (hashset_t)(opaque_ptr(opq));
    hashset_destroy(hs);
    return_closcall1(data, k, boolean_t);
  ")

(define-c hs-num-items
  "(void *data, int argc, closure _, object k, object opq )"
  "
    hashset_t hs = (hashset_t)(opaque_ptr(opq));
    int count = hashset_num_items(hs);
    return_closcall1(data, k, obj_int2obj(count));
  ")

(define-c hs-add!
  "(void *data, int argc, closure _, object k, object opq, object item )"
  "
    hashset_t hs = (hashset_t)(opaque_ptr(opq));
    int rv = hashset_add(hs, item);
    return_closcall1(data, k, obj_int2obj(rv));
  ")

(define (hs-add-all! hs lis)
  (for-each
    (lambda (p)
      (hs-add! hs p))
    lis))

(define-c hs-remove!
  "(void *data, int argc, closure _, object k, object opq, object item )"
  "
    hashset_t hs = (hashset_t)(opaque_ptr(opq));
    int rv = hashset_remove(hs, item);
    return_closcall1(data, k, obj_int2obj(rv));
  ")

(define-c hs-member?
  "(void *data, int argc, closure _, object k, object opq, object item )"
  "
    hashset_t hs = (hashset_t)(opaque_ptr(opq));
    int rv = hashset_is_member(hs, item);
    return_closcall1(data, k, rv ? boolean_t : boolean_f);
  ")

))
