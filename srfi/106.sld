;; Sockets library
(define-library (srfi 106)
; TODO: include necessary C headers
; TODO: do we need to link to any libraries???
  (import (scheme base))
  (export
      make-client-socket make-server-socket socket?
      socket-accept socket-send socket-recv
      socket-shutdown socket-close
      socket-input-port
      socket-output-port
      call-with-socket
      address-family address-info 
      socket-domain ip-protocol
      message-type shutdown-method
      socket-merge-flags
      socket-purge-flags
      *af-unspec* *af-inet* *af-inet6*
      *sock-stream* *sock-dgram*
      *ai-canonname* *ai-numerichost*
      *ai-v4mapped* *ai-all* *ai-addrconfig*
      *ipproto-ip* *ipproto-tcp* *ipproto-udp*
      *msg-peek* *msg-oob* *msg-waitall*
      *shut-rd* *shut-wr* *shut-rdwr*
  )
  (begin
    (define *ipproto-ip* 0)
    (define *ipproto-tcp* 6)
    (define *ipproto-udp* 17)
; really want to define-C these, but just to specify a constant and not a whole function
; TODO:            (rename (AF_UNSPEC *af-unspec*)
; TODO:                    (AF_INET   *af-inet*)
; TODO:                    (AF_INET6  *af-inet6*))
; TODO:            (rename (SOCK_STREAM *sock-stream*)
; TODO:                    (SOCK_DGRAM  *sock-dgram*))
; TODO:            (rename (AI_CANONNAME   *ai-canonname*)
; TODO:                    (AI_NUMERICHOST *ai-numerichost*)
; TODO:                    (AI_V4MAPPED    *ai-v4mapped*)
; TODO:                    (AI_ALL         *ai-all*)
; TODO:                    (AI_ADDRCONFIG  *ai-addrconfig*))
; TODO:            (rename (MSG_PEEK     *msg-peek*)
; TODO:                    (MSG_OOB      *msg-oob*)
; TODO:                    (MSG_WAITALL  *msg-waitall*))
; TODO:            (rename (SHUT_RD   *shut-rd*)
; TODO:                    (SHUT_WR   *shut-wr*)
; TODO:                    (SHUT_RDWR *shut-rdwr*)))
  )
)
