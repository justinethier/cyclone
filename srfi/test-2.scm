(import (scheme base)
        (scheme write)
        (scheme cyclone pretty-print)
        (2))

(pretty-print `( 
  ,(and-let* ((x 1) (y 2))
     (+ x y))
  ,(and-let* ((x 1) (y 2) (#f))
     (+ x y))
))
