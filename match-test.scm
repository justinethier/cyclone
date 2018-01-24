(import 
  (scheme base) 
  (scheme write)
)
(cond-expand
  (cyclone
    (import
      (match-test-lib)
      (scheme cyclone test)))
  (chibi
    (import
      (chibi match)
      (chibi test)))
)

(test-group
  "official tests"

  (test 2 (match (list 1 2 3) ((a b c) b)) )
  (test 2 (match (list 1 2 1) ((a a b) 1) ((a b a) 2)))
  (test 1 (match (list 1 2 1) ((_ _ b) 1) ((a b a) 2)) )
  (test 2 (match 'a ('b 1) ('a 2)) )

;; fails on cyclone, works in chibi
;(display (match (list 1 2 3) (`(1 ,b ,c) (list b c))) )(newline)

  (test #t (match (list 1 2) ((1 2 3 ...) #t)) )
  (test #t (match (list 1 2 3) ((1 2 3 ...) #t)) )
  (test #t (match (list 1 2 3 3 3) ((1 2 3 ...) #t)) )
  (test '() (match (list 1 2) ((a b c ...) c)) )
  (test '(3) (match (list 1 2 3) ((a b c ...) c)) )
  (test '(3 4 5) (match (list 1 2 3 4 5) ((a b c ...) c)) )
  (test '() (match (list 1 2 3 4) ((a b c ... d e) c)) )
  (test '(3) (match (list 1 2 3 4 5) ((a b c ... d e) c)) )
  (test '(3 4 5) (match (list 1 2 3 4 5 6 7) ((a b c ... d e) c)) )

;; Next 2 fail on both cyclone and chibi
;;; Pattern not matched
;(display (match (list 1 2) ((a b c ..1) c)) )(newline)
;;; Should have matched??
;(display (match (list 1 2 3) ((a b c ..1) c)) )(newline)

;; Next 3 fail on cyclone but pass on chibi
;(display (match 1 ((and) #t)) )(newline)
;(display (match 1 ((and x) x)) )(newline)
;(display (match 1 ((and x 1) x)) )(newline)

  (test #f (match 1 ((or) #t) (else #f)) )

;; Next 2 fail on cyclone but pass on chibi
;(display (match 1 ((or x) x)) )(newline)
;(display (match 1 ((or x 2) x)) )(newline)

  (test #t (match 1 ((not 2) #t)) )

;; Fails on cyclone but passes on chibi
;(display (match 1 ((? odd? x) x)) )(newline)
  (test 1 (match '(1 . 2) ((= car x) x)) )
  (test 16 (match 4 ((= square x) x)) )
)
(test-exit)
