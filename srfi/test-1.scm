(import (scheme base)
        (scheme write)
        (scheme cyclone pretty-print)
        (1))

(pretty-print `( 
  ,(iota 10)
  ,(iota 10 5)
  ,(iota 10 5 10)
))
