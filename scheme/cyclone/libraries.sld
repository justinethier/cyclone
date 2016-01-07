;;
;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module implements r7rs libraries. In our compiler, these are used to
;; encapsulate C modules.
;;
;; Initially, this a quicky-and-dirty (for now) implementation of r7rs libraries.
;;
;; TODO: go through functions and ensure consistent naming conventions.
;;   probably should also clean up some of the function names, this is
;;   not a very clean or nice API at the moment.
;;
(define-library (scheme cyclone libraries)
  (import (scheme base)
          (scheme read)
          (scheme cyclone util)
  )
  (export
    library?
    lib:list->import-set
    lib:name
    lib:name->string
    lib:name->symbol
    lib:result
    lib:exports
    lib:imports
    lib:body
    lib:includes
    lib:import->filename
    lib:import->metalist
    lib:import->path
    lib:read-imports
    lib:import->export-list
    lib:resolve-imports
    lib:resolve-meta
    lib:get-all-import-deps
    lib:get-dep-list
  )
  (begin
  ;  (define read cyc-read)

(define (library? ast)
  (tagged-list? 'define-library ast))

;; Convert a raw list to an import set. For example, a list might be
;; (srfi 18) containing the number 18. An import set contains only symbols.
(define (lib:list->import-set lis)
  (map
    (lambda (atom)
      (cond
        ((number? atom)
         (string->symbol (number->string atom)))
        (else atom)))
    lis))

(define (lib:name ast) 
  (lib:list->import-set (cadr ast)))

;; Convert name (as list of symbols) to a mangled string
(define (lib:name->string name)
  (apply string-append (map mangle name)))

;; Convert library name to a unique symbol
(define (lib:name->symbol name)
  (string->symbol 
    (string-append
      "lib-init:" ;; Maybe make this an optional param? Trying to ensure uniqueness
      (lib:name->string name))))

;; Helper function that returns an empty list as a default value
(define (lib:result result)
  (if result result '()))

;; TODO: most of these below assume 0 or 1 instances of the directive.
;; may need to replace some of these later with filter operations to
;; support more than 1 instance.
(define (lib:exports ast)
  (lib:result 
    (let ((code (assoc 'export (cddr ast))))
      (if code (cdr code) #f))))
(define (lib:imports ast)
  (lib:result
    (let ((code (assoc 'import (cddr ast))))
      (if code (lib:list->import-set (cdr code)) #f))))
(define (lib:body ast)
  (lib:result
    (let ((code (assoc 'begin (cddr ast))))
      (if code (cdr code) #f))))
(define (lib:includes ast)
  (map
    (lambda (inc-lst)
      (cadr inc-lst))
    (filter
      (lambda (code)
        (tagged-list? 'include code))
      (cddr ast))))

;; TODO: include-ci, cond-expand

(define (lib:atom->string atom)
  (cond
    ((symbol? atom)
     (symbol->string atom))
    ((number? atom)
     (number->string atom))
    (else
     (error "Unexpected type in import set"))))

;; Resolve library filename given an import. 
;; Assumes ".sld" file extension if one is not specified.
(define (lib:import->filename import . ext)
  (let* ((file-ext 
          (if (null? ext)
              ".sld"
              (car ext)))
         (filename*
          (string-append
            (apply
              string-append
              (map 
                (lambda (i) 
                  (string-append "/" (lib:atom->string i)))
                import))
            file-ext))
         (filename
           (substring filename* 1 (string-length filename*))))
    (if (or (tagged-list? 'scheme import)
            (tagged-list? 'srfi import))
      (string-append (Cyc-installation-dir 'sld) "/" filename) ;; Built-in library
      filename)))

;; Get path to directory that contains the library
(define (lib:import->path import)
  (let* ((import-path (reverse (cdr (reverse import))))
         (path
           (apply
             string-append
             (map 
               (lambda (i) 
                 (string-append (lib:atom->string i) "/"))
               import-path))))
    (if (tagged-list? 'scheme import)
      (string-append (Cyc-installation-dir 'sld) "/" path) ;; Built-in library
      path)))

;; Given a program's import set, resolve each import to its .o file, then
;; process each import recursively to get the .o files that each one of those
;; libs requires. will probably need to prune duplicates from completed list.
;; Longer-term, do we want to look at file timestamps to see if files need to
;; be recompiled?
;(define (lib:imports->objs imports)
;  (apply
;    append
;    (map
;      (lambda (i)
;        (cons
;          (lib:import->filename i ".o")
;          (lib:imports->objs (lib:read-imports i))
;        ))
;      imports)))

;; Given a single import from an import-set, open the corresponding
;; library file and retrieve the library's import-set.
(define (lib:read-imports import)
  (let* ((dir (lib:import->filename import))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (imports (lib:imports (car lib))))
    (close-input-port fp)
    imports))

;; Read export list for a given import
(define (lib:import->export-list import)
  (let* ((dir (string-append (lib:import->filename import)))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (exports (lib:exports (car lib))))
    (close-input-port fp)
    exports))

;; Take a list of imports and resolve it to the imported vars
(define (lib:resolve-imports imports)
 (apply
   append
   (map 
     (lambda (import)
       (lib:import->export-list import))
     (lib:list->import-set imports))))

(define (lib:import->metalist import)
  (let ((file (lib:import->filename import ".meta"))
        (fp #f)
        (result '()))
    (cond
      ((file-exists? file)
       (set! fp (open-input-file file))
       (set! result (car (read-all fp)))
       (close-input-port fp)))
    result))

(define (lib:resolve-meta imports)
 (apply
   append
   (map 
     (lambda (import)
       (lib:import->metalist import))
     imports)))

;; Given an import set, get all dependant import names that are required
;; The list of deps is intended to be returned in order, such that the
;; libraries can be initialized properly in sequence.
(define (lib:get-all-import-deps imports)
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
   (lib:get-dep-list libraries/deps)
   ))

;; Given a list of alists (library-name . imports), return an ordered
;; list of library names such that each lib is encounted after the
;; libraries it imports (it's dependencies).
(define (lib:get-dep-list libs/deps)
 ; Overall strategy is:
 ; for each library
 ;  compute index of result that is after any libs that lib imports
 ;  compute index of result that is before any libs that import lib
 ;   if there is a 'hole' then insert lib into result in that space
 ;   otherwise, throw an error (unfortunate but will identify problems)
 ;
 ; To test, run this from hello directory: 
 ; (pp (lib:get-all-import-deps '((scheme base) (scheme eval) (scheme base)
 ;       (scheme read) (scheme eval) (libs lib1) (libs lib2))))
 ;
 (let ((result '()))
  (for-each
    (lambda (lib/dep)
      (cond
        ((null? result)
         (set! result (cons lib/dep '())))
        (else
          (let ((idx-my-imports 0) ; lib must be placed after this
                (idx-imports-me (length result))) ; lib must be before any libs that import it
            (define (loop i)
              (cond
                ((= i (length result))
                 'done)
                (else
                  ;; Does lib import this one?
                  (if (and
                        (> i idx-my-imports)
                        (member (car (list-ref result i)) (cdr lib/dep)))
                    (set! idx-my-imports i))

                  ;; Does this one import lib?
                  (if (and
                        (< i idx-imports-me)
                        (member (car lib/dep) (cdr (list-ref result i))))
                    (set! idx-imports-me i))

                  (loop (+ i 1)))))
            (loop 0)
            ;(pp `(JAE DEBUG ,result ,lib/dep ,idx-imports-me ,idx-my-imports))
            (if (<= idx-my-imports idx-imports-me)
              (list-insert-at! result lib/dep 
                (if (= idx-my-imports idx-imports-me)
                  idx-my-imports
                  (+ 1 idx-my-imports)))
              (error "Unable to import library, possibly due to a circular dependency:" lib/dep))))
        ))
    libs/deps)
  (map car result)))

))
