(define-library (srfi 18)
  (import (scheme base))
  (export
    thread?
    make-thread
    thread-name
    thread-specific
    thread-specific-set!
    thread-start!
    thread-yield!
;    thread-terminate!
    ; For now, these are built-ins. No need for them here: make-mutex mutex-lock! mutex-unlock!
    ->heap
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
; current-thread - not sure how to look this up yet... may need a global list of running threads
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
    (define-c ->heap
      "(void *data, int argc, closure _, object k, object obj)"
      " object heap_obj = copy2heap(data, obj);
        return_closcall1(data, k, heap_obj); ")
))
