(define-library (srfi 18)
  (import (scheme base))
  (export
    ;; TODO: current-thread
    thread?
    make-thread
    thread-name
    thread-specific
    thread-specific-set!
    thread-start!
    thread-sleep!
    thread-yield!
    ;; TODO: thread-terminate!
    ;; TODO: thread-join!

    ;; For now, these are built-ins. No need to export them here: 
    ;; mutex?
    ;; make-mutex 
    ;; mutex-lock! 
    ;; mutex-unlock!

    ;; For now, these are not implemented:
    ;; mutex-name
    ;; mutex-specific
    ;; mutex-specific-set!
    ;; mutex-state

    ;; TODO: condition variables are not implemented yet
    ;; (condition-variable? obj)                             ;procedure
    ;; (make-condition-variable [name])                      ;procedure
    ;; (condition-variable-name condition-variable)          ;procedure
    ;; (condition-variable-specific condition-variable)      ;procedure
    ;; (condition-variable-specific-set! condition-variable obj) ;procedure
    ;; (condition-variable-signal! condition-variable)       ;procedure
    ;; (condition-variable-broadcast! condition-variable)    ;procedure

    ;; Time functions are not implemented here, see (scheme time) instead
   
    ;; Exceptions are not implemented here, r7rs exceptions are used instead

    ;; Non-standard functions:
    ->heap
    Cyc-minor-gc
  )
  (begin
    ;; Threading
    (define (thread? obj) 
      (and (vector? obj) 
           (> (vector-length obj) 0) 
           (equal? 'cyc-thread-obj (vector-ref obj 0))))

    (define (make-thread thunk . name)
      (let ((name-str (if (pair? name)
                          (car name)
                          "")))
        ;; Fields supported so far:
        ;; - type marker (implementation-specific)
        ;; - thunk
        ;; - internal thread id (implementation-specific)
        ;; - name
        ;; - specific
        (vector 'cyc-thread-obj thunk #f name-str #f)))

    (define (thread-name t) (vector-ref t 3))
    (define (thread-specific t) (vector-ref t 4))
    (define (thread-specific-set! t obj) (vector-set! t 4 obj))

    ; TODO:
    ; current-thread - not sure how to look this up yet... may need a global list of running threads. Unfortunately need the vector here

    (define (thread-start! t)
      ;; Initiate a GC prior to running the thread, in case
      ;; t contains any closures on the "parent" thread's stack
      (Cyc-minor-gc)
      (let* ((thunk (vector-ref t 1)) 
             (mutator-id (Cyc-spawn-thread! thunk)))
        (vector-set! t 2 mutator-id)))
    (define (thread-yield!) (thread-sleep! 1))
;    (define (thread-terminate!) (Cyc-end-thread!))
    ;; TODO: thread-join!

    (define-c thread-sleep!
      "(void *data, int argc, closure _, object k, object timeout)"
      " return_closcall1(data, k, Cyc_thread_sleep(data, timeout)); ")

    ;; Take a single object and if it is on the stack, return a copy
    ;; of it that is allocated on the heap. NOTE the original object
    ;; will still live on the stack, and will eventually be moved
    ;; itself to the heap if it is referenced during minor GC.
    (define-c ->heap
      "(void *data, int argc, closure _, object k, object obj)"
      " object heap_obj = copy2heap(data, obj);
        return_closcall1(data, k, heap_obj); ")
    ;; Trigger a minor garbage collection.
    ;; This is potentially useful to evacuate all objects from 
    ;; a thread's stack to the heap.
    (define-c Cyc-minor-gc
      "(void *data, int argc, closure _, object k)"
      " Cyc_trigger_minor_gc(data, k); ")
))
