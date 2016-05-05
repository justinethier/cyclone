;; Temporary file to test changes to the optimization library
(import (optimize-cps)
        (ast)
        (scheme write)
        (scheme base))

;(adb:init!)
(adb:set! 'v (adb:make-var))

(let ((v (adb:get 'v)))
  (adbv:set-global! v #t)
  (adbv:set-defined-by! v 1)

  (display v)
  (newline)
  (display (adbv:defined-by v))
  (newline))


(define l (ast:make-lambda '() '((write 1) (write 2) (write 3))))
(write l)
(newline)
(set! l (ast:make-lambda '() '((write 1) (write 2) (write 3))))
(write `(l is now ,l))
(newline)
