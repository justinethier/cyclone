;; Another temporary test file
(import (scheme base) (scheme write))
 
(define (bbs-lbl-counter bbs) (vector-ref bbs 0))
(define label-limit 9999)
(define (bbs-new-lbl! bbs) ((bbs-lbl-counter bbs)))
(define (queue-empty) (cons '() '()))
(define (make-bbs)
  (vector (make-counter 1 label-limit bbs-limit-err) (queue-empty) '()))
(define (make-counter next limit limit-error)
  (lambda ()
    (if (< next limit)
        (let ((result next)) (set! next (+ next 1)) result)
        (limit-error))))
(define (bbs-limit-err)
  (error "procedure is too long [too many labels]"))

(let* (
             (bbs (make-bbs))
             (lbl1 (bbs-new-lbl! bbs))
             (lbl2 (bbs-new-lbl! bbs))
      )
;      (let* ((p-bbs *bbs*)
;             (p-bb *bb*)
;             (p-proc-queue proc-queue)
;             (p-known-procs known-procs)
;             (p-context (current-context))
;             (bbs (make-bbs))
;             (lbl1 (bbs-new-lbl! bbs))
;             (lbl2 (bbs-new-lbl! bbs))
;             (context (entry-context node '()))
;             (frame (context->frame
;                     context
;                     (set-union (free-variables (prc-body node)) ret-var-set)))
;             (bb1 (make-bb (make-label-entry
;                            lbl1
;                            (length (prc-parms node))
;                            (prc-min node)
;                            (prc-rest node)
;                            #f
;                            frame
;                            (source-comment node))
;                           bbs))
;             (bb2 (make-bb (make-label-simple lbl2 frame (source-comment node))
;                           bbs)))
(write `(DEBUG bbs ,bbs))
(newline)
)
