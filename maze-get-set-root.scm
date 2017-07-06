;;; MAZE -- Constructs a maze on a hexagonal grid, written by Olin Shivers.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (get-set-root s)
  (let lp ((r s));; Find the last pair
    (let ((next (cdr r)));; in the list. That's
      (cond ((pair? next) (lp next));; the root r.

            (else
             (if (not (eq? r s));; Now zip down the list again,
                 (let lp ((x s));; changing everyone's cdr to r.
                   (let ((next (cdr x)))
                     (cond ((not (eq? r next))
                            (set-cdr! x r)
                            (lp next))))))
             r)))));; Then return r.

(write
  (get-set-root '()))
