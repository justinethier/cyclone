(define-library (cyclone concurrency)
;; TODO:
;how to determine if an object can be shared between threads (EG: on the heap)?
;
;what data type to use for concurrency objects?
;what data structures to support - queue, ??
;what operations to support - promises, ??
;what objects to support - atomics, refs, ??
;
;what role do atomics play? how does that affect GC?

;; Some additional notes:
;; Add atomic bit field to cvar, maybe even an atomic type field too (?)
;; 
;; (Make-shared exprs)
;; Safe way to allocate vars on heap. Tbd if an obj with pointers always necessitates a minor GC 
;;
 (import
   (scheme base)
 )
 (export
   immutable?
 )
 (begin
   (define (dummy) #f)
   (define-c immutable?
     "(void *data, int argc, closure _, object k, object obj)"
     "object result = boolean_f;
      if (is_object_type(obj) &&
          (type_of(obj) == pair_tag ||
           type_of(obj) == vector_tag ||
           type_of(obj) == bytevector_tag ||
           type_of(obj) == string_tag
          ) &&
          immutable(obj) ) {
        result = boolean_t;
      }
      return_closcall1(data, k, result); ")
 )
)
