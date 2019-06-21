;; Notes:
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
(define-library (cyclone concurrent)
 (import
   (scheme base)
   (srfi 18)
 )
 (include-c-header "<ck_pr.h>")
 (export
   ;; Atoms
   make-atom
   atom
   atom?
   deref
   swap!
   compare-and-set!
   ;; Futures
   future?
   future
   future-call
   future-deref
   ;; Immutable objects
   immutable?
   ;; Shared objects
   make-shared
   share-all!
 )
 (begin

(define-c atom?
  "(void *data, int argc, closure _, object k, object obj)"
  " object result = Cyc_is_atomic(obj);
    return_closcall1(data, k, result); ")
;; 
;; Alloc on the heap since by definition atoms are used by multiple threads
;; 
;; We also enforce that an object added to an atom must be an immutable and shared (IE, not thread-local).
;; Because we enforce these guarantees:
;; - The only way for application-code to change the object is via atomic calls. No other synchronization is required
;; - Atoms do not need to have a write barrier because the atom's heap object can never refer to a stack object
;; - We do not need to travese an entire pair/vector to determine all members are shared. Only the first object needs to be checked.
(define-c %make-atom
  "(void *data, int argc, closure _, object k, object obj)"
  " int heap_grown;
    atomic atm;
    atomic_type tmp;
    Cyc_verify_immutable(data, obj); // TODO: verify obj is not on local stack???
    if (gc_is_stack_obj(data, obj)){
      Cyc_rt_raise2(data, \"Atom cannot contain a thread-local object\", obj);
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

;; - deref atomic
(define-c deref
  "(void *data, int argc, closure _, object k, object obj)"
  " atomic a;
    Cyc_check_atomic(data, obj);
    a = (atomic) obj;
    return_closcall1(data, k, ck_pr_load_ptr(&(a->obj)));")

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
  (let* ((oldval (deref atom))
         (newval (make-shared (apply f oldval args))))
    (if (compare-and-set! atom oldval newval)
        newval ;; value did not change, return new one
        (apply swap! atom f args) ;; Value changed, try again
        )))

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
      Cyc_rt_raise2(data, \"Atom cannot contain a thread-local object\", obj);
    }
    a = (atomic) obj;
    bool result = ck_pr_cas_ptr(&(a->obj), oldval, newval);
    object rv = result ? boolean_t : boolean_f;
    return_closcall1(data, k, rv); ")

;; Return a reference to an object that can be safely shared by many threads.
;; 
;; If the given object is atomic or already shared it it simply returned.
;; Otherwise it is necessary to create a copy of the object.
;;
;; Note this function may trigger a minor GC if a thread-local pair or vector 
;; is passed.
(define-c make-shared
  "(void *data, int argc, closure _, object k, object obj)"
  " Cyc_make_shared_object(data, k, obj); ")

;; Allow all objects currently on the calling thread's local stack to be shared
;; with other threads.
(define-c share-all!
  "(void *data, int argc, closure _, object k)"
  " Cyc_trigger_minor_gc(data, k); ")

;; Predicate - is the given object immutable?
(define-c immutable?
  "(void *data, int argc, closure _, object k, object obj)"
  "object result = boolean_t;
   if (is_object_type(obj) &&
       (type_of(obj) == pair_tag ||
        type_of(obj) == vector_tag ||
        type_of(obj) == bytevector_tag ||
        type_of(obj) == string_tag
       ) &&
       !immutable(obj) ) {
     result = boolean_f;
   }
   return_closcall1(data, k, result); ")

;; Futures
  (define-record-type <future>
    (make-future done result lock)
    future?
    (done get-done set-done!)
    (result get-result set-result!)
    (lock get-lock set-lock!))

  ;; macro: (future expr ...)
  (define-syntax future
    (er-macro-transformer
      (lambda (expr rename compare)
        `(future-call (lambda () ,@(cdr expr))))))

;; From the clojure docs:
;;
;; Takes a function of no args and yields a future object that will
;; invoke the function in another thread, and will cache the result and
;; return it on all subsequent calls to deref/@. If the computation has
;; not yet finished, calls to deref/@ will block, unless the variant
;; of deref with timeout is used. See also - realized?.
(define (future-call thunk)
  (let* (
         (lock (make-mutex))
         (ftr (make-future #f #f lock))
         (tfnc (lambda ()
                 (mutex-lock! lock) 
                 (let ((result (thunk))) ;; TODO: Catch exceptions (?)
                   (set-result! ftr result)
                   (set-done! ftr #t)
                   (mutex-unlock! lock) 
                 )))
         (t (make-thread tfnc))
        )
    (thread-start! t)
    ftr))

;;(define (future-done? ftr)
;;  (when (not (future? ftr))
;;    (error "Expected future but received" ftr))
;; TODO: may be a good candidate for a timed mutex lock, just return #f if minimum timeout is exceeded
;;)

;; TODO: (future-cancel ftr)
;; TODO: (future-cancelled? ftr)

;;TODO: custom deref but eventually need to fold this functionality back into the main one
(define (future-deref ftr)
  (when (not (future? ftr))
    (error "Expected future but received" ftr))
  (let ((result #f))
    (mutex-lock! (get-lock ftr))
    (set! result (get-result ftr))
    (mutex-unlock! (get-lock ftr))
    result))
;; END Futures

 )
)
