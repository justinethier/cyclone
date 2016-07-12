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

;; Goal is to resolve a list of dependencies into the appropriate order such 
;; that no node is encountered before its dependencies.
;; We also need to raise an error if a circular dependency is found
;; 
;; A dependency list consists of: (name . edges)
;; Where edges are all of the dependencies of name.
;; 
;; nodes is a list of many dependency lists.
;;
;; Based on code from:
;; http://www.electricmonk.nl/log/2008/08/07/dependency-resolving-algorithm/
(define (resolve-dependencies nodes)
  (define (append-cell! cell value) (set-cdr! cell (cons value (cdr cell))))
  (define (make-cell) (cons #f '()))
  (define get-cell cdr)
  (define (node->edges name) (assoc name nodes))

  ;; Create a new node that depends on all the others
  (define (master-dependency)
    (cons '(#f) (map car nodes)))

  (define (dep-resolve node resolved seen)
    ; DEBUG: (write node) (newline)
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
    resolved)

  (set! nodes (cons (master-dependency) nodes))
  (let* ((resolved (dep-resolve (node->edges '(#f)) (make-cell) (make-cell)))
         (deps (reverse (cdr (get-cell resolved))))) ;; cdr to get rid of master list
    (map car deps)))

(write (resolve-dependencies test2))
(newline)
(write (resolve-dependencies *test*))

;  (set! test2 (cons (get-all-names) test2))
;;(write (get-all-names))
;;(newline)
;(dep-resolve (node->edges '(all)) result seen)
;;(dep-resolve (node->edges '(a)) result seen)
;(newline)
;(write (reverse (get-cell result)))

