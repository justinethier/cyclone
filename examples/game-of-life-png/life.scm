;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; The game of life example from r7rs. 
;;; Main program
;;;
;;; To execute from the husk directory:
;;;
;;;  > cd examples/game-of-life
;;;  > huski life.scm
;;;
(import (scheme base)
        (example life)
        (example grid))
;; TODO:
;        (only (example life) life)
;        (rename (prefix (example grid) grid-)
;                (grid-make make-grid)))

;; Simple example of including headers in a program.
;; Just place them here in the top-level, after 
;; the (import) expression, if any.
(include-c-header "stdlib.h")
(include-c-header "<stdio.h>")
;; END C headers

;; Initialize a grid with a glider.
;(define grid (make-grid 24 24))
;(grid-put! grid 1 1 #t)
;(grid-put! grid 2 2 #t)
;(grid-put! grid 3 0 #t)
;(grid-put! grid 3 1 #t)
;(grid-put! grid 3 2 #t)
(define grid (make 24 24))
(put! grid 1 1 #t)
(put! grid 2 2 #t)
(put! grid 3 0 #t)
(put! grid 3 1 #t)
(put! grid 3 2 #t)

(put! grid 11 11 #t)
(put! grid 12 12 #t)
(put! grid 13 10 #t)
(put! grid 13 11 #t)
(put! grid 13 12 #t)

(put! grid 6 6 #t)
(put! grid 7 7 #t)
(put! grid 8 5 #t)
(put! grid 8 6 #t)
(put! grid 8 7 #t)

(put! grid 1 11 #t)
(put! grid 2 12 #t)
(put! grid 3 10 #t)
(put! grid 3 11 #t)
(put! grid 3 12 #t)

(put! grid 15 0 #t)
(put! grid 15 1 #t)
(put! grid 16 1 #t)
(put! grid 16 2 #t)
(put! grid 17 2 #t)
(put! grid 17 3 #t)
(put! grid 18 2 #t)
(put! grid 18 3 #t)
;; Run for x iterations.
(life grid 100)
