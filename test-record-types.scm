;; Temporary test file!!
(import (scheme base) (scheme write))
      (define-record-type employee
        (make-employee name title)
        employee?
        (name get-name)
        (title get-title))
      (define-record-type employee2
        (make-employee2 name title)
        employee2?
        (name get-name2)
        (title get-title2))
(let ((e (make-employee "Bob" "Doctor")))
  (newline)(display (employee? e))
  (newline)(display (employee2? e))
  (newline)(display e)
  (newline)(display (vector-ref e 0))
  (newline)(display (vector-ref e 1))
  (newline)(display (vector-ref e 2))
  ;; Why is name false after this, instead of 'employee??
  (newline)(display ((make-constructor "make-employee" employee)))
  (newline)(display employee)
  (newline)(display (is-a? e employee))
  (newline)(display (is-a? e employee2))
)

(define (is-a? obj rtype)
  (and (record? obj)
       (record? rtype)
       (equal? (vector-ref obj 1) rtype)))
