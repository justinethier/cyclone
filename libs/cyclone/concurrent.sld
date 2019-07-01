;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; A library for writing concurrent programs using Cyclone.
;;;;
(define-library (cyclone concurrent)
 (import
   (scheme base)
   (srfi 18)
   (scheme write) ;; TODO: debugging only!
 )
 (include-c-header "<ck_pr.h>")
 (export
   ;; Generic Concurrency
   deref
   ;; Atoms
   make-atom
   atom
   atom?
   swap!
   compare-and-set!
   atom-deref
   ;; Futures
   future?
   future
   future-call
   future-deref
   future-done?
   ;; Shared Queues
   shared-queue?
   make-shared-queue
   shared-queue
   shared-queue-add!
   shared-queue-remove!
   shared-queue-clear!
   shared-queue-size
   shared-queue-wait-count
   shared-queue-capacity
   shared-queue-empty?
   ;; Thread Pool
   make-thread-pool 
   thread-pool?
   thread-pool-size
   thread-pool-idling-count
   thread-pool-idling?
   thread-pool-push-task!
   ;thread-pool-release!
   ;; Immutable objects
   immutable?
   ;; Shared objects
   make-shared
   share-all!
 )
 (begin

;; Dereference the given concurrency object
(define (deref obj)
  (cond
    ((atom? obj) (atom-deref obj))
    ((future? obj) (future-deref obj))
    (else obj)))

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
(define-c atom-deref
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
                   (set-result! ftr (make-shared result))
                   (set-done! ftr #t)
                   (mutex-unlock! lock) 
                 )))
         (t (make-thread tfnc))
        )
    (thread-start! t)
    ftr))

(define (future-done? ftr)
  (when (not (future? ftr))
    (error "Expected future but received" ftr))
  (let ((result (mutex-lock! (get-lock ftr) 0.01))) ;; Not ideal but short block on failure
    (if result (mutex-unlock! (get-lock ftr))) ;; Ensure mutex is always  not want to hold mutex longunlocked
    (if result #t #f))) ;; Bit awkward, but ensure boolean result

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

;; Shared Queues
;;
;; Each is a vector containing a circular buffer of objects that are intended
;; to be shared among many threads. All operations are locked and thread-safe,
;; and the queue will ensure any objects added are made into shared objects for
;; use by other threads.
;;
;; Removal from a queue is a blocking operation, so threads can easily wait for
;; new data to arrive.
(define *default-sq-table-size* 64)

(define-record-type <shared-queue>
  (%make-shared-queue store start end wait-count lock cv)
  shared-queue?
  (store q:store q:set-store!)
  (start q:start q:set-start!)
  (end q:end q:set-end!)
  (wait-count q:wait-count q:set-wait-count)
  (lock q:lock q:set-lock!)
  (cv q:cv q:set-cv!)
  )

(define (make-shared-queue)
  (make-shared
    (%make-shared-queue
      (make-vector *default-sq-table-size* #f)
      0
      0
      0
      (make-mutex)
      (make-condition-variable)
      )))

(define (shared-queue . elems)
  (let ((q (make-shared-queue)))
    (for-each
      (lambda (elem)
        (%shared-queue-add! q elem))
      (reverse elems))))

;; Increment an index, possibly back around to the beginning of the queue
(define (inc index capacity)
  (if (= index (- capacity 1))
      0
      (+ index 1)))

;; Inner add, assumes we already have the lock
(define (%shared-queue-add! q obj)
  (vector-set! (q:store q) (q:end q) (make-shared obj))
  (q:set-end! q (inc (q:end q) (vector-length (q:store q))))
  (when (= (q:start q) (q:end q))
     (%shared-queue-resize! q))
)

(define (shared-queue-add! q obj)
  (mutex-lock! (q:lock q))
  (%shared-queue-add! q (make-shared obj))
  (mutex-unlock! (q:lock q))
  (condition-variable-signal! (q:cv q))
)

(define (%shared-queue-resize! q)
 ;; (write "TODO: resize the queue")(newline)
  ;; TODO: assumes we already have the lock
  ;; TODO: error if size is larger than fixnum??
  (let* ((old-start (q:start q))
         (old-end (q:end q))
         (old-store (q:store q))
         (new-store (make-vector (* (vector-length old-store) 2) #f)))
    (q:set-start! q 0)
    (q:set-end! q 0)
    (q:set-store! q new-store)
    (let loop ((i 0)
               (start old-start))
      (when (not (= i (vector-length old-store)))
        (%shared-queue-add! q (vector-ref old-store start))
        (loop (+ i 1) (inc start (vector-length old-store))))))
)

;; Blocks if queue is empty (!)
;; should we have a failsafe if the same thread that is doing adds, then
;; does a blocking remove??
(define (shared-queue-remove! q)
  (let loop ((waiting #f))
    (mutex-lock! (q:lock q))
    ;; If thread was previously waiting, clear that status
    (when waiting
      (set! waiting #f)
      (q:set-wait-count q (- (q:wait-count q) 1)))

    (cond
      ((= (q:start q) (q:end q))
       ;; Let Q know we are waiting
       (set! waiting #t)
       (q:set-wait-count q (+ (q:wait-count q) 1))
       ;; Wait for CV, indicating data is ready
       (mutex-unlock! (q:lock q) (q:cv q))
       (loop waiting))
      (else
        (let ((result (vector-ref (q:store q) (q:start q))))
          (q:set-start! q (inc (q:start q) (vector-length (q:store q))))
          (mutex-unlock! (q:lock q))
          result)))))

(define (shared-queue-clear! q)
  (mutex-lock! (q:lock q))
  (q:set-start! q 0)
  (q:set-end! q 0)
  (mutex-unlock! (q:lock q)))

(define (shared-queue-wait-count q)
  (define result 0)
  (mutex-lock! (q:lock q))
  (set! result (q:wait-count q))
  (mutex-unlock! (q:lock q))
  result)

;; Return current length of the queue
(define (shared-queue-size q)
  (define result 0)
  (mutex-lock! (q:lock q))
  (set! result (%shared-queue-size q))
  (mutex-unlock! (q:lock q))
  result)

(define (%shared-queue-size q)
  (let ((start (q:start q))
        (end (q:end q))
        (capacity (vector-length (q:store q))))
    (cond
      ((< end start) (+ (- capacity start) end))
      ((> end start) (- end start))
      (else 0)))) ;; (= end start)

(define (shared-queue-empty? q)
  (= 0 (shared-queue-size q)))

;; Return max size of the queue (until resize occurs)
(define (shared-queue-capacity q)
  (define result 0)
  (mutex-lock! (q:lock q))
  (set! result (vector-length (q:store q)))
  (mutex-unlock! (q:lock q))
  result)

;- shared-queue->list

;; END Shared Queues

;; Thread Pool
  (define-record-type <thread-pool>
    (%make-thread-pool jobq threads )
    thread-pool?
    (jobq tp:jobq tp-set-jobq!)
    (threads tp:threads tp:set-threads!)
    )

(define (thread-pool-default-handler err) 
;; TODO: why is this never being called??
(write "called default error handler") (newline)
#f)

(define (%make-thread-pool-thread q handler)
  (make-thread 
    (lambda ()
        (let loop ()
          (with-handler
            handler
            (let ((thunk (shared-queue-remove! q)))
              (thunk))
          )
          (loop)
        ))))

(define (make-thread-pool size . opts)
  (let ((tp (%make-thread-pool (make-shared-queue) '() size))
        (handler (if (and (pair? opts)
                          (procedure? (car opts)))
                     (car opts)
                     thread-pool-default-handler)))
    (do ((i size (- i 1))) 
        ((zero? i)) 
      (let ((t (%make-thread-pool-thread (tp:jobq tp) (make-shared handler))))
        (tp:set-threads! tp (cons t (tp:threads tp)))
        (thread-start! t)))
    (share-all!)
    tp))

(define (thread-pool-size tp)
  (shared-queue-size (tp:jobq tp)))

(define (thread-pool-idling-count tp)
  (shared-queue-wait-count (tp:jobq tp)))

(define (thread-pool-idling? tp)
  (> (thread-pool-idling-count tp) 0))

(define (thread-pool-push-task! tp thunk)
  (shared-queue-add! (tp:jobq tp) (make-shared thunk)))

;; Stop all thread pool threads, effectively GC'ing the thread pool
;; For now just uses thread-terminate for this purpose. The theory being that each 
;; thread is not supposed to hold its own state anyway, and the TP is finishing up,
;; so do not anticipate this termination method causing any problems with (EG) 
;; orphaned resources, etc.
(define (thread-pool-release! tp)
  (let ((terminate (lambda () (thread-terminate! (current-thread)))))
    (for-each
     (lambda (thread)
       ;; Force each thread to terminate
       (thread-pool-push-task! tp terminate))
     (tp:threads tp))))

; ?? - thread-pool-wait-all!

;; END Thread Pool

 )
)
