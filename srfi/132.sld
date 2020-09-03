(define-library (srfi 132)



  (import (except (scheme base) vector-copy vector-copy!)
          (rename (only (scheme base) vector-copy vector-copy! vector-fill!)
                  (vector-copy  r7rs-vector-copy)
                  (vector-copy! r7rs-vector-copy!)
                  (vector-fill! r7rs-vector-fill!)
                  ))

 ; (import (scheme base))
  (import (scheme cxr)
          (srfi 27))
  (export list-sorted? vector-sorted? list-merge vector-merge list-sort vector-sort
          list-stable-sort vector-stable-sort list-merge! vector-merge! list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median! vector-find-median
          vector-select! vector-select
          vector-separate!
          )
  (include "sorting/delndups.scm")
  (include "sorting/lmsort.scm")
  (include "sorting/sortp.scm")
  (include "sorting/vector-util.scm")
  (include "sorting/vhsort.scm")
  (include "sorting/visort.scm")
  (include "sorting/vmsort.scm")
  (include "sorting/vqsort2.scm")
  (include "sorting/vqsort3.scm")
  (include "sorting/select.scm")
  (include "sorting/sort.scm") ; must be last
)
