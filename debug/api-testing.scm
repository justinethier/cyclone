(import 
  (scheme base)
  (scheme file)
  (scheme read)
  (scheme write)
  (scheme cyclone util)
)

;; TODO: this was not working in icyc - wtf?
;(let ((tmp (call-with-input-file "../scheme/base.sld" 
;                                (lambda (fp) 
;                                  (read-all fp)))))
;  (write
;   (cdar 
;    (filter 
;      (lambda (l) 
;        (tagged-list? 'export l)) 
;      (car tmp)))))

(define (read-exports filename)
  (let* ((tmp (call-with-input-file filename
                                  (lambda (fp) 
                                    (read-all fp))))
         (exports (cdar 
                    (filter 
                      (lambda (l) 
                        (tagged-list? 'export l)) 
                      (car tmp)))))
    (write
    (map
      (lambda (e)
        (system 
          (string-append
          ;; TODO: not good enough, what about define-syntax?
            "grep -n \"define[ ]*[ \\(]"
            (symbol->string e)
            " \" " filename)))
      exports)))
)
(read-exports "../scheme/file.sld")
