(import (scheme base) (scheme write))

(write #u8(1 2 3 4 5))
(write (make-bytevector 2 12)) ; =⇒ #u8(12 12)
(write (bytevector 1 3 5 1 3 5)) ; =⇒ #u8(1 3 5 1 3 5)
(write (bytevector)) ;=⇒ #u8()

(write (bytevector-append
          (make-bytevector 1 1)
          (make-bytevector 2 2)
          (make-bytevector 3 3)
))

(write (bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5)) ;=⇒ 8
(write
  (let ((bv (bytevector 1 2 3 4)))
    (bytevector-u8-set! bv 1 3)
    bv)
) ;=⇒ #u8(1 3 3 4)

(define a #u8(1 2 3 4 5))
(write (Cyc-bytevector-copy a 2 4)) ; =⇒ #u8(3 4)

