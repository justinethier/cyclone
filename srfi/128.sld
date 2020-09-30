(define-library (srfi 128) ;comparators)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme char) (scheme complex) (scheme inexact))
  (inline
    boolean<?)
  (export comparator? comparator-ordered? comparator-hashable?)
  (export make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator)
  (export boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash symbol-hash number-hash)
  (export make-default-comparator default-hash comparator-register-default!)
  (export comparator-type-test-predicate comparator-equality-predicate
        comparator-ordering-predicate comparator-hash-function)
  (export comparator-test-type comparator-check-type comparator-hash)
  (export hash-bound hash-salt)
  (export =? <? >? <=? >=?)
  (export comparator-if<=>)
  (export %salt%) ;; Temporary workaround since we cannot preserve referential transparency for this
  (export comparator-max-in-list comparator-min-in-list
          comparator-max comparator-min
          default-comparator
          boolean-comparator
          real-comparator
          char-comparator
          char-ci-comparator
          string-comparator
          string-ci-comparator
          pair-comparator
          list-comparator
          vector-comparator
          eq-comparator
          eqv-comparator 
          equal-comparator)
  (include "comparators/comparators-impl.scm")
  (include "comparators/default.scm")
  (include "comparators/162-impl.scm")
)
