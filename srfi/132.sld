(define-library (srfi 132)
  (import (scheme base))
  (import (scheme cxr))
  (export list-sorted? vector-sorted? list-merge vector-merge list-sort vector-sort
          list-stable-sort vector-stable-sort list-merge! vector-merge! list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!)
  (include "sorting/delndups.scm")
  (include "sorting/lmsort.scm")
  (include "sorting/sortp.scm")
  (include "sorting/vector-util.scm")
  (include "sorting/vhsort.scm")
  (include "sorting/visort.scm")
  (include "sorting/vmsort.scm")
  (include "sorting/vqsort2.scm")
  (include "sorting/vqsort3.scm")
  (include "sorting/sort.scm") ; must be last
)
