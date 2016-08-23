(define-library (srfi 111)
  (import (scheme base))
  (export box box? unbox set-box!)
  (begin
    (define-record-type box-type
      (box value)
      box?
      (value unbox set-box!))))
