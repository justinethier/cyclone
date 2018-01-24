(import 
  (scheme base) 
  (scheme write)
  (match-test-lib)
  ;(chibi match)
)

(display (match (list 1 2 3) ((a b c) b)) )(newline)
(display (match (list 1 2 1) ((a a b) 1) ((a b a) 2)))(newline)
(display (match (list 1 2 1) ((_ _ b) 1) ((a b a) 2)) )(newline)
(display (match 'a ('b 1) ('a 2)) )(newline)

;; fails on cyclone, works in chibi
;(display (match (list 1 2 3) (`(1 ,b ,c) (list b c))) )(newline)

(display (match (list 1 2) ((1 2 3 ...) #t)) )(newline)
(display (match (list 1 2 3) ((1 2 3 ...) #t)) )(newline)
(display (match (list 1 2 3 3 3) ((1 2 3 ...) #t)) )(newline)
(display (match (list 1 2) ((a b c ...) c)) )(newline)
(display (match (list 1 2 3) ((a b c ...) c)) )(newline)
(display (match (list 1 2 3 4 5) ((a b c ...) c)) )(newline)
(display (match (list 1 2 3 4) ((a b c ... d e) c)) )(newline)
(display (match (list 1 2 3 4 5) ((a b c ... d e) c)) )(newline)
(display (match (list 1 2 3 4 5 6 7) ((a b c ... d e) c)) )(newline)

;; Next 2 fail on both cyclone and chibi
;;; Pattern not matched
;(display (match (list 1 2) ((a b c ..1) c)) )(newline)
;;; Should have matched??
;(display (match (list 1 2 3) ((a b c ..1) c)) )(newline)

;; Next 3 fail on cyclone but pass on chibi
;(display (match 1 ((and) #t)) )(newline)
;(display (match 1 ((and x) x)) )(newline)
;(display (match 1 ((and x 1) x)) )(newline)

(display (match 1 ((or) #t) (else #f)) )(newline)

;; Next 2 fail on cyclone but pass on chibi
;(display (match 1 ((or x) x)) )(newline)
;(display (match 1 ((or x 2) x)) )(newline)

(display (match 1 ((not 2) #t)) )(newline)

;; Fails on cyclone but passes on chibi
;(display (match 1 ((? odd? x) x)) )(newline)
(display (match '(1 . 2) ((= car x) x)) )(newline)
(display (match 4 ((= square x) x)) )(newline)
