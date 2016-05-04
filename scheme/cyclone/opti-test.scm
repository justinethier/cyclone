;; Temporary file to test changes to the optimization library
(import (optimize-cps)
        (scheme write)
        (scheme base))

(adb:init!)
(adb:set! 'v (adb:make-var))

(let ((v (adb:get 'v)))
  (adbv:set-global! v #t)
  (adbv:set-defined-by! v 1)

  (display v)
  (newline)
  (display (adbv:defined-by v))
  (newline))
