;;
;; Basic example of using a custom type
;;
(import (scheme base)
        (scheme write)
        (cyclone foreign))

(c-define-type string-as-integer string number->string string->number)
(c-define scm-strlen int "strlen" string-as-integer)

(write
  (scm-strlen 42)
)
(newline)
