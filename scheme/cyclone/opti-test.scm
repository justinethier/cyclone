;; Temporary file to test changes to the optimization library
;(import (optimize-cps)
;        (scheme write)
;        (scheme base))
;
;(adb:init!)
;(adb:set! 'v (adb:make-var))
;
;(let ((v (adb:get 'v)))
;  (adbv:set-global! v #t)
;  (adbv:set-defined-by! v 1)
;
;  (display v)
;  (newline)
;  (display (adbv:defined-by v))
;  (newline))

(import (scheme cyclone libraries) (scheme write) (scheme base))

(define (test imports)
  (letrec ((libraries/deps '())
         (find-deps! 
          (lambda (import-sets)
            (for-each 
              (lambda (i)
                (let ((import-set (lib:list->import-set i)))
                  (cond
                   ;; Prevent cycles by only processing new libraries
                   ((not (assoc import-set libraries/deps))
                    ;; Find all dependencies of i (IE, libraries it imports)
                    (let ((deps (lib:read-imports import-set))) 
                     (set! libraries/deps (cons (cons import-set deps) libraries/deps))
                     (find-deps! deps)
                   )))))
              import-sets))))
   (find-deps! imports)
   ;`((deps ,libraries/deps) ; DEBUG
   ;  (result ,(lib:get-dep-list libraries/deps)))
   libraries/deps ;(lib:get-dep-list libraries/deps)
   (write libraries/deps)
   (newline)
   (lib:get-dep-list libraries/deps)
   ))

(write
    (test '((optimize-cps) (scheme write) (scheme base))))
(newline)
(write
    (lib:get-dep-list `(((scheme write) (scheme base)) ((scheme complex) (scheme base)) ((scheme char) (scheme base)) ((srfi 69) (scheme base) (scheme char) (scheme complex)) ((scheme base)) ((optimize-cps) (scheme base) (srfi 69)))))
;                      (((scheme write) (scheme base)) ((scheme complex) (scheme base)) ((scheme char) (scheme base)) ((srfi 69) (scheme base) (scheme char) (scheme complex)) ((scheme base)) ((optimize-cps) (scheme base) (srfi 69)))
(newline)
