;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module implements the basic socket interface from SRFI 106:
;;;; http://srfi.schemers.org/srfi-106/srfi-106.html
;;;;
(define-library (srfi 106)
  (include-c-header "<sys/types.h>")
  (include-c-header "<sys/socket.h>")
  (include-c-header "<netinet/in.h>")
  (include-c-header "<arpa/inet.h>")
  (include-c-header "<netdb.h>")
  (include-c-header "<unistd.h>")
  (include-c-header "<errno.h>")
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
  (inline
    socket->fd)
  (begin
    (define *socket-object-type* '%socket-object-type%)
    (define (socket->fd obj) (cdr obj))
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

    (define (make-server-socket service . opts)
      (let ((family *af-inet*)
            (socktype *sock-stream*)
            (proto *ipproto-ip*))
        (when (not (null? opts))
          (set! family (car opts))
          (when (> (length opts) 1)
            (set! socktype (cadr opts))
            (when (> (length opts) 2)
              (set! proto (caddr opts)))))
        (let ((sock-fd (%make-server-socket service family socktype proto)))
          (cons *socket-object-type* sock-fd))))

    (define-c %make-server-socket
      "(void *data, int argc, closure _, object k, 
        object aservice, object family, object socktype, object proto)"
      "
        int sockfd = 0;
        struct addrinfo hints, *servinfo, *p;
        int rv;
        const char *service = string_str(aservice);
        
        memset(&hints, 0, sizeof hints);
        hints.ai_family = obj_obj2int(family);
        hints.ai_socktype = obj_obj2int(socktype);
        hints.ai_protocol = obj_obj2int(proto);
        hints.ai_flags = AI_PASSIVE; // use my IP address
        
        if ((rv = getaddrinfo(NULL, service, &hints, &servinfo)) != 0) {
            char buffer[1024];
            snprintf(buffer, 1023, \"getaddrinfo: %s\", gai_strerror(rv));
            Cyc_rt_raise_msg(data, buffer);
        }
        
        // loop through all the results and bind to the first we can
        for(p = servinfo; p != NULL; p = p->ai_next) {
            if ((sockfd = socket(p->ai_family, p->ai_socktype,
                    p->ai_protocol)) == -1) {
                // perror(\"socket\");
                continue;
            }
        
            if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
                close(sockfd);
                //perror(\"bind\");
                continue;
            }
        
            break; // if we get here, we must have connected successfully
        }
        
        if (p == NULL) {
            // looped off the end of the list with no successful bind
            Cyc_rt_raise_msg(data, \"failed to bind socket\");
        }
        
        freeaddrinfo(servinfo); // all done with this structure

        if (listen(sockfd, 20) < 0) {
            Cyc_rt_raise_msg(data, \"Unable to listen on socket\");
        }
        return_closcall1(data, k, obj_int2obj(sockfd)); ")

    ;; See: http://beej.us/guide/bgnet/output/html/singlepage/bgnet.html#accept
    (define (socket-accept sock)
      (when (not (socket? sock))
        (error "Expected socket but received" sock))

      (let ((sockfd (%socket-accept (socket->fd sock))))
        (if (= sockfd -1)
            (error "An error occurred accepting a socket connection")
            (cons *socket-object-type* sockfd))))

    (define-c %socket-accept
      "(void *data, int argc, closure _, object k, object sockfd)"
      " int new_fd;
        struct sockaddr_storage their_addr;
        socklen_t addr_size;
        addr_size = sizeof(their_addr);

        set_thread_blocked(data, k);
        errno = 0;
        new_fd = accept(obj_obj2int(sockfd), (struct sockaddr *)&their_addr, &addr_size);
        //if (new_fd < 0){
        //  // TODO: not so good. maybe we should build a string and send that if an error occurs
        //  fprintf(stderr, \"errno = %d\\n\", errno);
        //}
        return_thread_runnable(data, obj_int2obj(new_fd)); ")

    (define (socket-send sock bv . opts)
      (let ((flags 0))
        (if (not (null? opts))
            (set! flags (car opts)))
        (%socket-send (socket->fd sock) bv flags)))

    (define-c %socket-send
      "(void *data, int argc, closure _, object k, object sockfd, object bvobj, object flags)"
      " // TODO: type checking 
        int bytes_sent;
        bytevector_type *bv =  (bytevector_type *)bvobj;
        bytes_sent = send(obj_obj2int(sockfd), bv->data, bv->len, obj_obj2int(flags));
        return_closcall1(data, k, obj_int2obj(bytes_sent));")

    (define (socket-recv sock size . opts)
      (let ((flags 0))
        (if (not (null? opts))
            (set! flags (car opts)))
        (%socket-recv (socket->fd sock) size flags)))

    (define-c %socket-recv
      "(void *data, int argc, closure _, object k, object sockfd, object size, object flags)"
      " // TODO: type checking 
        int len = obj_obj2int(size);
        object bv;
        alloc_bytevector(data, bv, len);
        ((bytevector)bv)->data = alloca(sizeof(char) * len);

        set_thread_blocked(data, k);
        ((bytevector)bv)->len = recv(obj_obj2int(sockfd), ((bytevector)bv)->data, len, obj_obj2int(flags));
        return_thread_runnable(data, bv);
      ")

    (define (socket-shutdown sock how)
      (if (and (socket? sock) (integer? how))
          (%socket-shutdown (socket->fd sock) how)))

    (define-c %socket-shutdown
      "(void *data, int argc, closure _, object k, object sockfd, object how)"
      " shutdown(obj_obj2int(sockfd), obj_obj2int(how));
        return_closcall1(data, k, boolean_t);")

    (define (socket-close sock)
      (if (socket? sock)
          (%socket-close (socket->fd sock))))

    (define-c %socket-close
      "(void *data, int argc, closure _, object k, object sockfd)"
      " close(obj_obj2int(sockfd));
        return_closcall1(data, k, boolean_t);")

;; TODO: may not be good enough, socket may be closed if file is closed
    (define (socket-input-port sock)
      (%socket-input-port (socket->fd sock)))

    (define-c %socket-input-port
      "(void *data, int argc, closure _, object k, object sockfd)"
      " FILE *fp = fdopen(obj_obj2int(sockfd), \"r\");
        make_input_port(port, fp, 1);
        return_closcall1(data, k, &port);")

    (define (socket-output-port sock)
      (%socket-output-port (socket->fd sock)))

    (define-c %socket-output-port
      "(void *data, int argc, closure _, object k, object sockfd)"
      " FILE *fp = fdopen(obj_obj2int(sockfd), \"w\");
        make_port(port, fp, 0);
        return_closcall1(data, k, &port);")

    (define (call-with-socket socket proc)
      (let ((result (proc socket)))
        (socket-close socket)
        result))

    (define-syntax flags:sym->const
      (er-macro-transformer
        (lambda (expr rename compare)
          `(define-syntax ,(cadr expr)
             (er-macro-transformer
               (lambda (expr rename compare)
                 (case (cadr expr)
                  ,@(cddr expr)
                  (else
                    (error 
                      "Unexpected value" 
                      (list (quote ,(cadr expr)) (cadr expr)))))))))))

    (flags:sym->const
      address-family
      ((inet)   '*af-inet*)
      ((inet6)  '*af-inet6*)
      ((unspec) '*af-unspec*))

    (flags:sym->const
      address-info
      ((canoname)     '*ai-canonname*)
      ((numerichost)  '*ai-numerichost*)
      ((v4mapped)     '*ai-v4mapped*)
      ((all)          '*ai-all*)
      ((addrconfig)   '*ai-addrconfig*))

    (flags:sym->const
      socket-domain
      ((stream)   '*sock-stream*)
      ((datagram) '*sock-dgram*))
      
    (flags:sym->const
      ip-protocol
      ((ip)  '*ipproto-ip*)
      ((tcp) '*ipproto-tcp*)
      ((udp) '*ipproto-udp*))
      
    (flags:sym->const
      message-type
      ((none)     0)
      ((peek)     '*msg-peek*)
      ((oob)      '*msg-oob*)
      ((wait-all) '*msg-waitall*))
      
    (flags:sym->const
      shutdown-method
      ((read)  '*shut-rd*)
      ((write) '*shut-wr*))

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
    ;; The next 2 are not defined on all platforms:
    (define *ai-v4mapped* (ai-v4mapped))
    (define-c ai-v4mapped
      "(void *data, int argc, closure _, object k)"
      " 
#ifdef AI_V4MAPPED
      return_closcall1(data, k, obj_int2obj(AI_V4MAPPED)); 
#else
      return_closcall1(data, k, obj_int2obj(0)); 
#endif
      ")
    (define *ai-all* (ai-all))
    (define-c ai-all
      "(void *data, int argc, closure _, object k)"
      " 
#ifdef AI_ALL
      return_closcall1(data, k, obj_int2obj(AI_ALL)); 
#else
      return_closcall1(data, k, obj_int2obj(0)); 
#endif
      ")
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
