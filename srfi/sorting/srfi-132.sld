(define-library (srfi-132)
  (import (scheme base))
  (import (scheme cxr))
  (export list-sorted? vector-sorted? list-merge vector-merge list-sort vector-sort
          list-stable-sort vector-stable-sort list-merge! vector-merge! list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!)
  (include "delndups.scm")
  (include "lmsort.scm")
  (include "sortp.scm")
  (include "vector-util.scm")
  (include "vhsort.scm")
  (include "visort.scm")
  (include "vmsort.scm")
  (include "vqsort2.scm")
  (include "vqsort3.scm")
  (include "sort.scm") ; must be last
)
