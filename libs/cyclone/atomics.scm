;; A temporary test file, let's try to get an API going here before writing too much support code
;; 
;; see: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/compare-and-set!
;; initial operations to support:
;; - create atomic
;; - ref atomic
;; - swap, see https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/swap!
;; - compare and swap?
;; 
;; 
;; once that starts going, double-back to how to allocate shared objects effectively.
;; probably want a (make-shared)
;; may also way a way to allocate multiple shared objects at once (since a minor GC will likely be req'd)

;; (atom? obj)
;; (atom obj)

(import (scheme base) (scheme write))
(include-c-header "<ck_pr.h>")

(define-c atom?
  "(void *data, int argc, closure _, object k, object obj)"
  " object result = Cyc_is_atomic(obj);
    return_closcall1(data, k, result); ")
;; 
;; Alloc on the heap since by definition atoms are used by multiple threads
;; 
(define-c make-atom
  "(void *data, int argc, closure _, object k, object obj)"
  " int heap_grown;
    atomic atm;
    atomic_type tmp;
    tmp.hdr.mark = gc_color_red;
    tmp.hdr.grayed = 0;
    tmp.tag = atomic_tag;
    tmp.obj = obj;
    atm = gc_alloc(((gc_thread_data *)data)->heap, sizeof(atomic_type), (char *)(&tmp), (gc_thread_data *)data, &heap_grown);
    ck_pr_store_ptr(&(atm->obj), obj); // Needed??
    return_closcall1(data, k, atm); ")

(define (atom . obj)
  (if (pair? obj)
      (make-atom (car obj))
      (make-atom #f)))

;; - ref atomic
(define-c ref
  "(void *data, int argc, closure _, object k, object obj)"
  " atomic a;
    if (Cyc_is_atomic(obj) != boolean_t) {
      Cyc_rt_raise2(data, \"Type error: expected atom\", obj);
    }
    a = (atomic) obj;
    return_closcall1(data, k, ck_pr_load_ptr(&(a->obj)));")

;; TODO:
;; - swap, see https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/swap!
;; (swap! atom f)(swap! atom f x)(swap! atom f x y)(swap! atom f x y & args)

;; TODO:
;; - compare and swap?

(define a (make-atom '(1 2)))
(write
  (list
    a
    (ref a)))
