#|
 | Copyright (c) 2016 John Cowan
 | Copyright (c) 2017 Koz Ross 
 |
 | Permission is hereby granted, free of charge, to any person obtaining a copy of
 | this software and associated documentation files (the "Software"), to deal in
 | the Software without restriction, including without limitation the rights to
 | use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 | the Software, and to permit persons to whom the Software is furnished to do so,
 | subject to the following conditions:
 |
 | The above copyright notice and this permission notice shall be included in all
 | copies or substantial portions of the Software.
 |
 | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 | FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 | COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 | IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 | CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#

(define (any pred ls)
  (cond
    ((null? ls) #f)
    ((pred (car ls)) #t)
    (else (any pred (cdr ls)))))

(define (every pred ls)
  (not (any (lambda (x) (not (pred x)))) ls))

(define (generator . args)
  (lambda () 
    (if (null? args)
        (eof-object)
        (let ((next (car args)))
          (set! args (cdr args))
          next))))

(define make-iota-generator
  (case-lambda 
    ((count) (make-iota-generator count 0 1))
    ((count start) (make-iota-generator count start 1))
    ((count start step) (make-iota count start step))))

(define (make-iota count start step)
  (lambda ()
    (if (<= count 0) ;; in case someone passes a negative count
        (eof-object)
        (let ((result start))
          (set! count (- count 1))
          (set! start (+ start step))
          result))))

(define make-range-generator
  (case-lambda
    ((start) (make-infinite-range start))
    ((start end) (make-range-generator start end 1))
    ((start end step) (make-range start end step))))

(define (make-range start end step)
  (set! start (- (+ start step) step))
  (lambda ()
    (if (< start end)
        (let ((v start))
          (set! start (+ start step))
          v)
        (eof-object))))

(define (make-infinite-range start)
  (lambda ()
    (let ((result start))
     (set! start (+ start 1))
     result)))

(define (make-coroutine-generator proc)
  (define return #f)
  (define resume #f)
  (define yield (lambda (v) 
                  (call/cc (lambda (r) 
                             (set! resume r) (return v)))))
  (lambda () 
    (call/cc (lambda (cc) (set! return cc)
               (if resume
                   (resume (if #f #f)) ;; call resume with undefined
                   (begin (proc yield)
                     (set! resume (lambda (v) (return (eof-object))))
                     (return (eof-object))))))))

(define (list->generator lst)
  (lambda () 
    (if (null? lst)
        (eof-object)
        (let ((next (car lst)))
          (set! lst (cdr lst))
          next))))

;; NOTE: This, and similar, functions, should really be macro'd away.
(define vector->generator
  (case-lambda 
    ((vec) (vector->generator vec 0 (vector-length vec)))
    ((vec start) (vector->generator vec start (vector-length vec)))
    ((vec start end)
     (lambda () 
       (if (>= start end)
           (eof-object)
           (let ((next (vector-ref vec start)))
             (set! start (+ start 1))
             next))))))

(define reverse-vector->generator
  (case-lambda 
    ((vec) (reverse-vector->generator vec 0 (vector-length vec)))
    ((vec start) (reverse-vector->generator vec start (vector-length vec)))
    ((vec start end)
     (lambda () 
       (if (>= start end)
           (eof-object)
           (let ((next (vector-ref vec (- end 1))))
             (set! end (- end 1))
             next))))))

;; NOTE: Under UTF-8 semantics, this is O(n^2) rather than O(n)
;; Should be rewritten using cursors or something
(define string->generator
  (case-lambda 
    ((str) (string->generator str 0 (string-length str)))
    ((str start) (string->generator str start (string-length str)))
    ((str start end)
     (lambda () 
       (if (>= start end)
           (eof-object)
           (let ((next (string-ref str start)))
             (set! start (+ start 1))
             next))))))

(define bytevector->generator
  (case-lambda 
    ((str) (bytevector->generator str 0 (bytevector-length str)))
    ((str start) (bytevector->generator str start (bytevector-length str)))
    ((str start end)
     (lambda () 
       (if (>= start end)
           (eof-object)
           (let ((next (bytevector-u8-ref str start)))
             (set! start (+ start 1))
             next))))))

(define (make-for-each-generator for-each obj)
  (make-coroutine-generator (lambda (yield) (for-each yield obj))))

(define (make-unfold-generator stop? mapper successor seed)
  (make-coroutine-generator (lambda (yield)
                              (let loop ((s seed))
                               (if (stop? s)
                                 (if #f #f)
                                 (begin (yield (mapper s))
                                        (loop (successor s))))))))

(define (gcons* . args)
  (lambda ()
    (cond
      ((null? args) (eof-object))
      ((null? (cdr args)) ((car args)))
      (else (let ((v (car args)))
              (set! args (cdr args))
              v)))))

(define (gappend . args)
  (lambda () 
    (if (null? args)
        (eof-object)
        (let loop ((v ((car args))))
          (if (eof-object? v)
              (begin (set! args (cdr args))
                (if (null? args)
                    (eof-object)
                    (loop ((car args)))))
              v)))))

(define (gcombine proc seed . gens)
  (lambda ()
    (define items (map (lambda (x) (x)) gens))
    (if (any eof-object? items)
      (eof-object)
      (begin
        (receive (value newseed) (apply proc (append items (list seed)))
          (set! seed newseed)
          value)))))

(define (gfilter pred gen)
  (lambda () 
    (let loop ()
      (let ((next (gen)))
        (if (or (eof-object? next)
                (pred next))
                 next
                 (loop))))))

(define (gremove pred gen)
  (gfilter (lambda (v) (not (pred v))) gen))

(define gtake
  (case-lambda 
    ((gen k) (gtake gen k (eof-object)))
    ((gen k padding)
     (make-coroutine-generator 
       (lambda (yield)
         (if (> k 0)
             (let loop ((i 0) (v (gen)))
               (begin 
                 (if (eof-object? v) (yield padding) (yield v))
                 (if (< (+ 1 i) k)
                     (loop (+ 1 i) (gen))
                     (eof-object))))
             (eof-object)))))))


(define (gdrop gen k)
  (lambda () (do () ((<= k 0)) (set! k (- k 1)) (gen))
    (gen)))

(define (gdrop-while pred gen)
  (define found #f)
  (lambda ()
    (let loop ()
     (let ((val (gen)))
      (cond (found val)
            ((and (not (eof-object? val)) (pred val)) (loop))
            (else (set! found #t) val))))))

(define (gtake-while pred gen)
  (lambda () 
    (let ((next (gen)))
      (if (eof-object? next)
          next
          (if (pred next)
              next
              (begin (set! gen (generator))
                (gen)))))))

(define gdelete
  (case-lambda 
    ((item gen) (gdelete item gen equal?))
    ((item gen ==) 
     (lambda () 
       (let loop ((v (gen)))
         (cond
           ((eof-object? v) (eof-object))
           ((== item v) (loop (gen)))
           (else v)))))))

(define gdelete-neighbor-dups
  (case-lambda 
    ((gen) (gdelete-neighbor-dups gen equal?))
    ((gen ==)
     (define firsttime #t)
     (define prev #f)
     (lambda () 
       (if firsttime
           (begin (set! firsttime #f)
             (set! prev (gen))
             prev)
           (let loop ((v (gen)))
             (cond
               ((eof-object? v)
                v)
               ((== prev v)
                (loop (gen)))
               (else
                 (set! prev v)
                 v))))))))

(define (gindex value-gen index-gen)
  (let ((done? #f) (count 0))
   (lambda ()
     (if done?
       (eof-object)
       (let loop ((value (value-gen)) (index (index-gen)))
        (cond
          ((or (eof-object? value) (eof-object? index))
           (set! done? #t)
           (eof-object))
          ((= index count)
           (set! count (+ count 1))
           value)
          (else
            (set! count (+ count 1))
            (loop (value-gen) index))))))))

(define (gselect value-gen truth-gen)
  (let ((done? #f))
   (lambda ()
     (if done?
       (eof-object)
       (let loop ((value (value-gen)) (truth (truth-gen)))
        (cond
          ((or (eof-object? value) (eof-object? truth))
           (set! done? #t)
           (eof-object))
          (truth value)
          (else (loop (value-gen) (truth-gen)))))))))

(define generator->list
  (case-lambda 
    ((gen n) (generator->list (gtake gen n)))
    ((gen) (reverse (generator->reverse-list gen)))))

(define generator->reverse-list
  (case-lambda 
    ((gen n) (generator->reverse-list (gtake gen n)))
    ((gen) (generator-fold cons '() gen))))

(define generator->vector
  (case-lambda 
    ((gen) (list->vector (generator->list gen)))
    ((gen n) (list->vector (generator->list gen n)))))

(define (generator->vector! vector at gen)
  (let loop ((value (gen)) (count 0) (at at))
   (cond
     ((eof-object? value) count)
     ((>= at (vector-length vector)) count)
     (else (begin
             (vector-set! vector at value)
             (loop (gen) (+ count 1) (+ at 1)))))))

(define generator->string
  (case-lambda 
    ((gen) (list->string (generator->list gen)))
    ((gen n) (list->string (generator->list gen n)))))

(define (generator-fold f seed . gs)
  (define (inner-fold seed)
    (let ((vs (map (lambda (g) (g)) gs)))
     (if (any eof-object? vs)
       seed
       (inner-fold (apply f (append vs (list seed)))))))
  (inner-fold seed))

(define (generator-for-each f . gs)
  (let loop ()
   (let ((vs (map (lambda (g) (g)) gs)))
    (if (any eof-object? vs)
      (if #f #f)
      (begin (apply f vs)
             (loop))))))

(define (generator-find pred g)
  (let loop ((v (g)))
   (cond
     ((pred v) v)
     ((eof-object? v) #f)
     (else (loop (g))))))

(define (generator-count pred g)
  (generator-fold (lambda (v n) (if (pred v) (+ 1 n) n)) 0 g))

(define (generator-any pred g)
  (let loop ((v (g)))
    (cond
      ((eof-object? v) #f)
      ((pred v) #t)
      (else (loop (g))))))

(define (generator-every pred g)
  (not (generator-any (lambda (x) (not (pred x))) g)))

(define (generator-unfold g unfold . args)
  (apply unfold eof-object? (lambda (x) x) (lambda (x) (g)) (g) args))
