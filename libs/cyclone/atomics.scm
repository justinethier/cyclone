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
(define-c %make-atom
  "(void *data, int argc, closure _, object k, object obj)"
  " int heap_grown;
    atomic atm;
    atomic_type tmp;
    Cyc_verify_immutable(data, obj); // TODO: verify obj is not on local stack???
    if (gc_is_stack_obj(data, obj)){
      Cyc_rt_raise2(data, \"Atom cannot be a thread-local object\", obj);
    }
    tmp.hdr.mark = gc_color_red;
    tmp.hdr.grayed = 0;
    tmp.tag = atomic_tag;
    tmp.obj = obj;
    atm = gc_alloc(((gc_thread_data *)data)->heap, sizeof(atomic_type), (char *)(&tmp), (gc_thread_data *)data, &heap_grown);
    ck_pr_store_ptr(&(atm->obj), obj); // Needed??
    return_closcall1(data, k, atm); ")

(define (make-atom obj)
  (%make-atom (make-shared obj)))

(define (atom . obj)
  (if (pair? obj)
      (%make-atom (make-shared (car obj)))
      (%make-atom #f)))

;; - ref atomic
(define-c ref
  "(void *data, int argc, closure _, object k, object obj)"
  " atomic a;
    Cyc_check_atomic(data, obj);
    a = (atomic) obj;
    return_closcall1(data, k, ck_pr_load_ptr(&(a->obj)));")

;; TODO:
;; - swap, see https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/swap!
;; Clojure docs:
;; Atomically swaps the value of atom to be:
;; (apply f current-value-of-atom args). Note that f may be called
;; multiple times, and thus should be free of side effects.  Returns
;; the value that was swapped in.
;; (swap! atom f)(swap! atom f x)(swap! atom f x y)(swap! atom f x y & args)
;;
;; Notes:
;; swap! takes the current value of the Atom, calls the function on it (in this case, inc), and sets the new value. However, just before setting the new value, it checks to make sure the old value is still in there. If it's different, it starts over. It calls the function again with the new value it found. It keeps doing this until it finally writes the value. Because it can check the value and write it in one go, it's an atomic operation, hence the name.
;; 
;; That means the function can get called multiple times. That means it needs to be a pure function. Another thing is that you can't control the order of the function calls. If multiple threads are swapping to an Atom at the same time, order is out of the window. So make sure your functions are independent of order, like we talked about before.
;;
(define (swap! atom f . args)
  (let* ((oldval (ref atom))
         (newval (make-shared (apply f oldval args))))
    (if (compare-and-set! atom oldval newval)
        newval ;; value did not change, return new one
        (apply swap! atom f args) ;; Value changed, try again
        )))

(define-c make-shared
  "(void *data, int argc, closure _, object k, object obj)"
  " Cyc_make_shared_object(data, k, obj); ")
;; TODO: (make-shared obj)
;; likely implemented in runtime.c, either needs obj to be an immedate or 
;; an obj without children we can move to the heap or
;; an object with children that we have to minor GC before it can be moved to the heap.
;; in the last case, how do we return a ref to the heap object?

;
;TODO: once swap works, need to figure out the strategy for handling thread-local and mutable objects.
;do we ensure an object is neither before being allowed to be added to an atom?
;
;  nice thing about enforcing immutabiility is we don't need to check an entire structure (pair, vec, bv) for members on the stack, we can just check the first element
;
;also need a process for bulk initialization of atoms, instead of forcing a GC for each init.
;
;TODO: need an internal version of this and an external one


;; (compare-and-set! atom oldval newval)
;; https://clojuredocs.org/clojure.core/compare-and-set!
;; Atomically sets the value of atom to newval if and only if the
;; current value of the atom is identical to oldval. Returns true if
;; set happened, else false
(define-c compare-and-set!
  "(void *data, int argc, closure _, object k, object obj, object oldval, object newval)"
  " atomic a;
    Cyc_check_atomic(data, obj);
    Cyc_verify_immutable(data, newval);
    if (gc_is_stack_obj(data, obj)){
      Cyc_rt_raise2(data, \"Atom cannot be a thread-local object\", obj);
    }
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
(newline)
(write
  (list
    (make-shared '(1 (2 (3))))
    (make-shared 1)
    (make-shared 1.0)
    (make-shared "this is a string")
    (make-shared #(a b c d))
    (make-shared #u8(1 2 3 4))
    ))
