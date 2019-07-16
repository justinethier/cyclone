(import 
  (scheme base) 
  (scheme write)
)
(cond-expand
  (cyclone
    (import
      (cyclone match)
      (cyclone test)))
  (chibi
    (import
      (chibi ast)
      (chibi match)
      (chibi test)))
)

(test-group
  "Official tests"

  (test 2 (match (list 1 2 3) ((a b c) b)) )
  (test 2 (match (list 1 2 1) ((a a b) 1) ((a b a) 2)))
  (test 1 (match (list 1 2 1) ((_ _ b) 1) ((a b a) 2)) )
  (test 2 (match 'a ('b 1) ('a 2)) )

  (test '(2 3) (match (list 1 2 3) (`(1 ,b ,c) (list b c))))

  (test #t (match (list 1 2) ((1 2 3 ...) #t)) )
  (test #t (match (list 1 2 3) ((1 2 3 ...) #t)) )
  (test #t (match (list 1 2 3 3 3) ((1 2 3 ...) #t)) )
  (test '() (match (list 1 2) ((a b c ...) c)) )
  (test '(3) (match (list 1 2 3) ((a b c ...) c)) )
  (test '(3 4 5) (match (list 1 2 3 4 5) ((a b c ...) c)) )
  (test '() (match (list 1 2 3 4) ((a b c ... d e) c)) )
  (test '(3) (match (list 1 2 3 4 5) ((a b c ... d e) c)) )
  (test '(3 4 5) (match (list 1 2 3 4 5 6 7) ((a b c ... d e) c)) )

;; Next fails on cyclone and chibi, I believe intentionally
;;; Pattern not matched
;(display (match (list 1 2) ((a b c ..1) c)) )(newline)

  (test '(3) (match (list 1 2 3) ((a b c ..1) c)))

  (test #t (match 1 ((and) #t)))
  (test 1 (match 1 ((and x) x)))
  (test 1 (match 1 ((and x 1) x)))

  (test #f (match 1 ((or) #t) (else #f)) )

  (test 1 (match 1 ((or x) x)))
;; Next fails on cyclone but pass on chibi
;(display (match 1 ((or x 2) x)) )(newline)

  (test #t (match 1 ((not 2) #t)) )

  (test 1 (match 1 ((? odd? x) x)))
  (test 1 (match '(1 . 2) ((= car x) x)) )
  (test 16 (match 4 ((= square x) x)) )

  (test '("Doctor" "Bob")
    (let ()
      (define-record-type employee
        (make-employee name title)
        employee?
        (name get-name)
        (title get-title))
      (match (make-employee "Bob" "Doctor")
        (($ employee n t) (list t n)))))

 (test '("Doctor" "Bob")
   (let ()
     (define-record-type employee
       (make-employee name title)
       employee?
       (name get-name)
       (title get-title))
     (match (make-employee "Bob" "Doctor")
       ((@ employee (title t) (name n)) (list t n)))))

  (test '(1 . 3) (let ((x (cons 1 2))) (match x ((1 . (set! s)) (s 3) x))))
  (test 2 (match '(1 . 2) ((1 . (get! g)) (g))))
  (test '(a a a) (match '(a (a (a b))) ((x *** 'b) x)))
  (test '(a c f) (match '(a (b) (c (d e) (f g))) ((x *** 'g) x)))

)

(test-group
  "Predicates"
  (test "test" (match "test" ((? string? s) s) (else #f)))

  (test #(fromlist 1 2) (match '(1 2) ((a b) (vector 'fromlist a b))))
  (test #f (match 42 (X #f)))
)

(define (calc-time lst)
  (match
   lst
   ;(() 0)
   (((? number? n) (or 's 'seconds 'sec) . rest)
    (+ 0 (* #e1 n) (calc-time rest)))
   (((? number? n) (or 'm 'min 'minutes) . rest)
    (+ (* #e60 n) (calc-time rest)))
   (((? number? n) (or 'hours 'h) . rest)
    (+ (* #e60 60 n) (calc-time rest)))
   (((? number? n) (or 'd 'days 'day) . rest)
    (+ (* #e60 60 24 n) (calc-time rest)))
   (((? number? n) (or 'w 'week 'weeks) . rest)
    (+ (* #e60 60 24 7 n) (calc-time rest)))
   (else 0)
))

(test-group
  "Demo"
  (test (+ (* 5 60) 10) (calc-time '(5 min 10 sec)))
  (test (+ (* 24 60 60) (* 60 5) 10) (calc-time '(1 day 5 min 10 sec)))
  (test (+ (* 24 60 60) (* 60 60) (* 60 5) 10) (calc-time '(0 weeks 1 day 1 h 5 min 10 sec)))
)

(test-exit)
