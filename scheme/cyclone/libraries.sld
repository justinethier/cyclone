;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module implements r7rs libraries. In our compiler, these are used to
;;;; encapsulate C modules.
;;;;
;;;; Initially, this a quicky-and-dirty (for now) implementation of r7rs libraries.
;;;;
;;;; TODO: go through functions and ensure consistent naming conventions.
;;;;   probably should also clean up some of the function names, this is
;;;;   not a very clean or nice API at the moment.
;;;;
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
    lib:rename-exports
    lib:imports
    lib:body
    lib:includes
    lib:include-c-headers
    lib:import->library-name
    lib:import->filename
    lib:import->metalist
    lib:import->path
    lib:read-imports
    lib:import->export-list
    lib:resolve-imports
    lib:resolve-meta
    lib:get-all
    lib:get-all-import-deps
    lib:get-dep-list
    lib:imports->idb
    lib:idb:ids
    lib:idb:id->import
  )
  (begin

(define (library? ast)
  (tagged-list? 'define-library ast))

;; Convert a raw list to an import set. For example, a list might be
;; (srfi 18) containing the number 18. An import set contains only symbols
;; or sub-lists.
(define (lib:list->import-set lis)
  (map
    (lambda (atom)
      (cond
        ((pair? atom)
         (lib:list->import-set atom))
        ((number? atom)
         (string->symbol (number->string atom)))
        (else atom)))
    lis))

(define (lib:name ast) 
  (lib:list->import-set (cadr ast)))

;; Convert name (as list of symbols) to a mangled string
(define (lib:name->string name)
  (apply string-append (map mangle (lib:import->library-name name))))

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

;; Get all instances of given tagged list from a library definition,
;; and collect the contents of them into a single list.
(define (lib:get-all ast tag)
  (foldr append '()
    (map cdr 
      (filter (lambda (l) (tagged-list? tag l)) (cddr ast)))))
(define (lib:body ast) 
  (lib:get-all ast 'begin))
(define (lib:imports ast) 
  (map lib:list->import-set (lib:get-all ast 'import)))
(define (lib:raw-exports ast) 
  (lib:get-all ast 'export))
(define (lib:rename-exports ast)
  (filter
    (lambda (ex)
      (tagged-list? 'rename ex))
    (lib:raw-exports ast)))
(define (lib:exports ast)
  (map
    (lambda (ex)
      ;; Replace any renamed exports
      (if (tagged-list? 'rename ex)
          (caddr ex)
          ex))
    (lib:raw-exports ast)))
(define (lib:includes ast)
  (map
    (lambda (inc-lst)
      (cadr inc-lst))
    (filter
      (lambda (code)
        (tagged-list? 'include code))
      (cddr ast))))

(define (lib:include-c-headers ast)
  (map
    (lambda (inc-lst)
      (cadr inc-lst))
    (filter
      (lambda (code)
        (tagged-list? 'include-c-header code))
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
  (let* ((lib-name (lib:import->library-name import))
         (dir (lib:import->filename lib-name))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (imports (lib:imports (car lib))))
    (close-input-port fp)
    imports))

(define (lib:import->library-name import)
  (cond
    ((or (tagged-list? 'only import)
         (tagged-list? 'except import))
     (cadr import))
    (else
     import)))

;; Read export list for a given import
(define (lib:import->export-list import)
  (let* ((lib-name (lib:import->library-name import))
         (dir (string-append (lib:import->filename lib-name)))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (exports (lib:exports (car lib))))
    (close-input-port fp)
    (cond
      ((tagged-list? 'only import)
       ;; Filter to symbols from "only" that appear in export list
       (filter
         (lambda (sym)
           (member sym exports))
         (cddr import)))
      ((tagged-list? 'except import)
       (filter
         (lambda (sym)
           (not (member sym (cddr import))))
         exports))
      (else 
       exports))))

;; Take a list of imports and resolve it to the imported vars
(define (lib:resolve-imports imports)
 (apply
   append
   (map 
     (lambda (import)
       (lib:import->export-list import))
     (map lib:list->import-set imports))))

;; Take a list of imports and create a "database" from them
;; consisting of maps between each exported identifier and the
;; library that imports that identifier. 
;;
;; TODO: Raise an exception if the same identifier is exported 
;; from more than one library???
;;
;; TODO: convert this to use a hashtable. Initially a-lists
;; will be used to prove out the concept, but this is inefficient
(define (lib:imports->idb imports)
 (apply
   append
   (map 
     (lambda (import-set)
       (let ((lib-name (lib:import->library-name import-set)))
         (foldr
           (lambda (id ids)
            (cons
              (cons id lib-name)
              ids))
          '()
           (lib:import->export-list import-set))))
     (map lib:list->import-set imports))))

;; Convert from the import DB to a list of identifiers that are imported.
;; EG: '((call/cc . (scheme base))) ==> '(call/cc)
(define (lib:idb:ids db)
  (foldr 
    (lambda (i is) (cons (car i) is))
   '() 
    db))

;; Map from identifier to the library that imported it
(define (lib:idb:id->import db identifier)
  (let ((entry (assoc identifier db)))
    (if entry
        (cdr entry)
        #f)))

(define (lib:import->metalist import)
  (let* ((lib-name (lib:import->library-name import))
         (file (lib:import->filename lib-name ".meta"))
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
                (let* ((import-set (lib:list->import-set i))
                       (lib-name (lib:import->library-name import-set)))
                  (cond
                   ;; Prevent cycles by only processing new libraries
                   ((not (assoc lib-name libraries/deps))
                    ;; Find all dependencies of i (IE, libraries it imports)
                    (let* ((deps (lib:read-imports import-set))
                           (dep-libs (map lib:import->library-name deps)))
                     (set! 
                       libraries/deps 
                       (cons (cons lib-name dep-libs) libraries/deps))
                     (find-deps! dep-libs)
                   )))))
              import-sets))))
   (find-deps! imports)
   ;`((deps ,libraries/deps)) ; DEBUG
   (lib:get-dep-list libraries/deps)
   ))

;; Given a list of alists (library-name . imports), return an ordered
;; list of library names such that each lib is encounted after the
;; libraries it imports (it's dependencies).
(define lib:get-dep-list resolve-dependencies)

;; Goal is to resolve a list of dependencies into the appropriate order such 
;; that no node is encountered before its dependencies.
;; We also need to raise an error if a circular dependency is found
;; 
;; A dependency list consists of: (name . edges)
;; Where edges are all of the dependencies of name.
;; 
;; nodes is a list of many dependency lists.
;;
;; Based on code from:
;; http://www.electricmonk.nl/log/2008/08/07/dependency-resolving-algorithm/
(define (resolve-dependencies nodes)
  (define (append-cell! cell value) (set-cdr! cell (cons value (cdr cell))))
  (define (make-cell) (cons #f '()))
  (define get-cell cdr)
  (define (node->edges name) (assoc name nodes))

  ;; Create a new node that depends on all the others
  (define (master-dependency)
    (cons '(#f) (map car nodes)))

  (define (dep-resolve node resolved seen)
    ; DEBUG: (write node) (newline)
    (append-cell! seen node)
    (for-each
      (lambda (edge)
        (cond
          ((not (assoc edge (get-cell resolved)))
           (if (assoc edge (get-cell seen))
               (error "Circular dependency detected" node edge))
           (dep-resolve (node->edges edge) resolved seen))))
      (cdr (node->edges (car node))))
    (append-cell! resolved node)
    resolved)

  (set! nodes (cons (master-dependency) nodes))
  (let* ((resolved (dep-resolve (node->edges '(#f)) (make-cell) (make-cell)))
         (deps (reverse (cdr (get-cell resolved))))) ;; cdr to get rid of master list
    (map car deps)))

))
