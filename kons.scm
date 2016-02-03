;; A temporary test file
(import (scheme base)
        (scheme write)
        (srfi 9))

(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

;(write
;  (list
;    (pare? (kons 1 2)) ; =. #t
;    (pare? (cons 1 2)) ; =. #f
;    (kar (kons 1 2))   ; =. 1
;    (kdr (kons 1 2))   ; =. 2
;    (let ((k (kons 1 2)))
;      (set-kar! k 3)
;      (kar k)))) ;=. 3

