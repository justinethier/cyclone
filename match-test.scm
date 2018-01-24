(import 
  (scheme base) 
  (scheme write)
  (match-test-lib)
)

(display (match (list 1 2 3) ((a b c) b)) )(newline)
(display (match (list 1 2 3) (`(1 ,b ,c) (list b c))) )(newline)
