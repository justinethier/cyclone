(define-library (srfi 133) ;vectors)
  (import (scheme base))
  (import (scheme cxr))
  (inline
    unspecified-value
    between?
    nonneg-int?
  )
  ;; Constructors 
  (export vector-unfold vector-unfold-right vector-reverse-copy 
          vector-concatenate vector-append-subvectors)
  ;; Predicates 
  (export vector-empty? vector=)
  ;; Iteration 
  (export vector-fold vector-fold-right vector-map!
          vector-count vector-cumulate)
  ;; Searching 
  (export vector-index vector-index-right vector-skip vector-skip-right 
          vector-binary-search vector-any vector-every vector-partition)
  ;; Mutators 
  (export vector-swap! vector-reverse! 
          vector-reverse-copy! vector-unfold! vector-unfold-right!)
  ;; Conversion 
  (export reverse-vector->list reverse-list->vector)
  (include "vectors-impl.scm")
)
