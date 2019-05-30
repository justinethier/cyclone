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
    Cyc_check_atomic(data, obj);
    a = (atomic) obj;
    return_closcall1(data, k, ck_pr_load_ptr(&(a->obj)));")

;; TODO:
;; - swap, see https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/swap!
;; (swap! atom f)(swap! atom f x)(swap! atom f x y)(swap! atom f x y & args)
(define (swap! atom f . opts)
  'TODO)

;; (compare-and-set! atom oldval newval)
;; https://clojuredocs.org/clojure.core/compare-and-set!
;; Atomically sets the value of atom to newval if and only if the
;; current value of the atom is identical to oldval. Returns true if
;; set happened, else false
(define-c compare-and-set!
  "(void *data, int argc, closure _, object k, object obj, object oldval, object newval)"
  " atomic a;
    Cyc_check_atomic(data, obj);
    a = (atomic) obj;
    bool result = ck_pr_cas_ptr(&(a->obj), oldval, newval);
    object rv = result ? boolean_t : boolean_f;
    return_closcall1(data, k, rv); ")

(define lis '(1 2))
(define a (make-atom lis))
(write
  (list
    a
    (ref a)
    (compare-and-set! a 1 lis)
    (ref a)
    (compare-and-set! a lis 1)
    (ref a)
))
