;; Copyright (c) 2004-2017 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A space efficient integer set (iset) implementation, optimized for
;;> minimal space usage and fast membership lookup.  General set
;;> operations are provided based on the character set operations
;;> found in SRFI-14.

(define-library (scheme cyclone iset)
  (export %make-iset
          make-iset
          iset?
          iset-contains?
          Integer-Set
          iset
          iset-copy
          list->iset
          list->iset!
          iset-adjoin
          iset-adjoin!
          iset-delete
          iset-delete!
          iset-union
          iset-union!
          iset-intersection
          iset-intersection!
          iset-difference
          iset-difference!
          iset-empty?
          iset-fold
          iset-fold-node
          iset-for-each
          iset-for-each-node
          iset-map
          iset->list
          iset-size
          iset=
          iset<=
          iset>=
          iset-cursor
          iset-cursor?
          iset-cursor-next
          iset-ref
          end-of-iset?)
  (import (scheme base)
          (scheme cyclone iset base)
          (scheme cyclone iset iterators)
          (scheme cyclone iset constructors)))

