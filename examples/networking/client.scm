(import (scheme base) (scheme write) (106))

(define s (make-client-socket "localhost" "12345"))
(socket-send s (string->utf8 "hello from client"))
(socket-close s)
