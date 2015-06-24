(import (scheme base)
        (scheme file)
        (scheme write))

(with-output-to-file 
  "test.out" 
  (lambda ()
    (write 'hello)
    (display " ")
    (display 'world)))
