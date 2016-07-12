(import (scheme base)
        (scheme write)
        (scheme cyclone libraries)
        )

(define *test* '(((scheme cyclone cps-optimizations) (scheme base) (scheme cyclone util) (scheme cyclone ast) (scheme cyclone primitives) (scheme cyclone transforms) (srfi 69)) ((scheme complex) (scheme base)) ((srfi 69) (scheme base) (scheme char) (scheme complex)) ((scheme cyclone pretty-print) (scheme base) (scheme write)) ((scheme cyclone macros) (scheme base) (scheme eval) (scheme cyclone util)) ((scheme cyclone libraries) (scheme base) (scheme read) (scheme cyclone util)) ((scheme cyclone transforms) (scheme base) (scheme char) (scheme eval) (scheme file) (scheme read) (scheme write) (scheme cyclone ast) (scheme cyclone common) (scheme cyclone libraries) (scheme cyclone macros) (scheme cyclone primitives) (scheme cyclone pretty-print) (scheme cyclone util) (srfi 69)) ((scheme cyclone primitives) (scheme base)) ((scheme cyclone cgen) (scheme base) (scheme char) (scheme write) (scheme cyclone primitives) (scheme cyclone transforms) (scheme cyclone util) (scheme cyclone libraries)) ((scheme cyclone common)) ((scheme cyclone ast) (scheme base) (scheme cyclone util)) ((scheme write) (scheme base)) ((scheme lazy) (scheme base)) ((scheme read) (scheme base) (scheme char)) ((scheme file) (scheme base)) ((scheme char) (scheme base)) ((scheme cyclone util) (scheme base) (scheme char)) ((scheme eval) (scheme cyclone util) (scheme base) (scheme file) (scheme read)) ((scheme case-lambda) (scheme base)) ((scheme base))))

(define test2
  '(((a) (b) (d))
    ((b) (c) (e))
    ((c) (d) (e))
    ((d))
    ;((d) (b)) ; circular dep!
    ((e))
   ))
;; TODO: see http://www.electricmonk.nl/log/2008/08/07/dependency-resolving-algorithm/

;; A dependency is a list (lib . deps)
;; lib is the name, and deps are the edges.
;; Goal is to resolve a list of dependencies into the appropriate order
;; We also need to raise an error if a circular dependency is found

;; TODO: consider loading all (lib . deps) into a table so we can easily get edges for a lib

(define (node->edges name)
  (assoc name test2))

(define result (cons #f '()))
(define seen (cons #f '()))

(define (append-cell! cell value)
  (set-cdr! cell (cons value (cdr cell))))
(define get-cell cdr)

(define (get-all-names)
  (cons '(all) (map car test2)))

(define (dep-resolve node resolved seen)
  ;(write (car node))
  (write node)
  (newline)
  (append-cell! seen node)
  (for-each
    (lambda (edge)
      (cond
        ((not (assoc edge (get-cell resolved)))
         (if (assoc edge (get-cell seen))
             (error "Circular dependency detected" node edge))
         (dep-resolve (node->edges edge) resolved seen))))
    (cdr (node->edges (car node))))
  (append-cell! resolved node)
)

(set! test2 (cons (get-all-names) test2))
;(write (get-all-names))
;(newline)
(dep-resolve (node->edges '(all)) result seen)
;(dep-resolve (node->edges '(a)) result seen)
(newline)
(write (reverse (get-cell result)))

