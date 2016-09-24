;; Sockets library
(define-library (106) ;(srfi 106)
  (include-c-header "<sys/types.h>")
  (include-c-header "<sys/socket.h>")
  (include-c-header "<netinet/in.h>")
  (include-c-header "<arpa/inet.h>")
  (include-c-header "<netdb.h>")
  (include-c-header "<unistd.h>")
  (import (scheme base) (scheme cxr))
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
    (define *socket-object-type* '%socket-object-type%)

    (define (socket? obj)
      (and (pair? obj) (eq? (car obj) *socket-object-type*)))

    (define (make-client-socket node service . opts)
      (let ((family *af-inet*)
            (socktype *sock-stream*)
            (flags (socket-merge-flags *ai-v4mapped* *ai-addrconfig*))
            (proto *ipproto-ip*))
        (when (not (null? opts))
          (set! family (car opts))
          (when (> (length opts) 1)
            (set! socktype (cadr opts))
            (when (> (length opts) 2)
              (set! flags (caddr opts))
              (when (> (length opts) 3)
                (set! proto (cadddr opts))))))
      (let ((sock-fd 
             (%make-client-socket node service family socktype flags proto)))
        (cons *socket-object-type* sock-fd))))

    ;; see: 
    ;; http://gnosis.cx/publish/programming/sockets.html
    ;; http://beej.us/guide/bgnet/output/html/multipage/getaddrinfoman.html
    (define-c %make-client-socket
      "(void *data, int argc, closure _, object k, 
        object anode, object aservice, 
        object family, object socktype, 
        object aflags, object protocol)"
      " int sockfd = 0;
        //struct sockaddr_in addr;
        struct addrinfo hints, *servinfo, *p;
        const char *node = string_str(anode),
                   *service = string_str(aservice);
        int rv, 
            flags = obj_obj2int(aflags),
            af = obj_obj2int(family),
            type = obj_obj2int(socktype),
            proto = obj_obj2int(protocol);

        memset(&hints, 0, sizeof hints);
        hints.ai_flags = flags;
        hints.ai_family = af;
        hints.ai_socktype = type;
        hints.ai_protocol = proto;

        if ((rv = getaddrinfo(node, service, &hints, &servinfo)) != 0) {
           char buffer[1024];
           snprintf(buffer, 1023, \"getaddrinfo: %s\", gai_strerror(rv));
           Cyc_rt_raise_msg(data, buffer);
        }

        // loop through all the results and connect to the first we can
        for(p = servinfo; p != NULL; p = p->ai_next) {
            if ((sockfd = socket(p->ai_family, p->ai_socktype,
                    p->ai_protocol)) == -1) {
                //perror(\"socket\");
                continue;
            }
        
            if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
                //perror(\"socket\");
                close(sockfd);
                continue;
            }
        
            break; // if we get here, we must have connected successfully
        }
        
        if (p == NULL) {
            // looped off the end of the list with no connection
            Cyc_rt_raise_msg(data, \"failed to connect client socket\");
        }
        
        freeaddrinfo(servinfo); // all done with this structure
        return_closcall1(data, k, obj_int2obj(sockfd)); ")

    (define-syntax address-family
      (er-macro-transformer
        (lambda (expr rename compare)
          (case (cadr expr)
            ((inet) '*af-inet*)
            ((inet6) '*af-inet6*)
            (else '*af-unspec*)))))

    (define (socket-merge-flags . flags)
      (if (null? flags) 
          0
          (let ((result (car flags)))
            (for-each 
              (lambda (flag)
                (set! result (bit-set result flag)))
              (cdr flags))
            result)))

    (define (socket-purge-flags base-flag . flags)
      (if (null? flags) 
          base-flag
          (begin
            (for-each 
              (lambda (flag)
                (set! base-flag (bit-unset base-flag flag)))
              flags)
              base-flag)))

    (define-c bit-set
      "(void *data, int argc, closure _, object k, object n1, object n2)"
      " return_closcall1(data, k, Cyc_bit_set(data, n1, n2));")

    (define-c bit-unset
      "(void *data, int argc, closure _, object k, object n1, object n2)"
      " return_closcall1(data, k, Cyc_bit_unset(data, n1, n2));")

    (define-syntax make-const
      (er-macro-transformer
        (lambda (expr rename compare)
          (let* ((base-str (symbol->string (cadr expr)))
                 (fsym (string->symbol (string-append "%" base-str "%")))
                 (vsym (string->symbol (string-append "*" base-str "*")))
                 (const-str (caddr expr)))
           `(begin
             (define ,vsym (,fsym))
             (define-c ,fsym
               "(void *data, int argc, closure _, object k)"
               ,(string-append 
                 "return_closcall1(data, k, obj_int2obj(" const-str ")); ")))))))

    (make-const af-unspec      "AF_UNSPEC"     )
    (make-const af-inet        "AF_INET"       )
    (make-const af-inet6       "AF_INET6"      )
    (make-const sock-stream    "SOCK_STREAM"   )
    (make-const sock-dgram     "SOCK_DGRAM"    )
    (make-const ai-canonname   "AI_CANONNAME"  )
    (make-const ai-numerichost "AI_NUMERICHOST")
    (make-const ai-v4mapped    "AI_V4MAPPED"   )
    (make-const ai-all         "AI_ALL"        )
    (make-const ai-addrconfig  "AI_ADDRCONFIG" )
    (make-const msg-peek       "MSG_PEEK"      )
    (make-const msg-oob        "MSG_OOB"       )
    (make-const msg-waitall    "MSG_WAITALL"   )
    (make-const shut-rd        "SHUT_RD"       )
    (make-const shut-wr        "SHUT_WR"       )
    (make-const shut-rdwr      "SHUT_RDWR"     )

    (define *ipproto-ip* 0)
    (define *ipproto-tcp* 6)
    (define *ipproto-udp* 17)
  )
)
