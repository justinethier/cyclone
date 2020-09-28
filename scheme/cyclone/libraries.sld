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
          ;; Debug only 
          (scheme write)
          (scheme read)
          (scheme process-context)
          (scheme cyclone util)
  )
  (export
    library?
    library-exists?
    lib:list->import-set
    lib:name
    lib:name->string
    lib:name->symbol
    lib:name->unique-string
    lib:result
    lib:exports
    lib:rename-exports
    lib:imports
    lib:body
    lib:cond-expand
    lib:cond-expand-decls
    lib:includes
    lib:include-c-headers
    lib:inlines
    lib:import-set:library-name?
    lib:import-set->import-set
    lib:import->library-name
    lib:import->filename
    lib:import->metalist
    lib:import->path
    lib:check-system-path
    lib:read-imports
    lib:read-includes
    lib:import->export-list
    lib:import-set/exports->imports
    ;lib:resolve-imports
    lib:resolve-meta
    lib:get-all
    lib:get-all-import-deps
    lib:get-dep-list
    lib:c-linker-options
    lib:read-c-linker-options
    lib:get-all-c-linker-options
    ;; Import Database "idb" oriented functions
    ;;
    ;; These functions perform operations for a "database" created from
    ;; the data taken from a list of import sets: imported objects,
    ;; renamed objects, and the libraries that contain them.
    lib:imports->idb
    lib:idb:ids
    lib:idb:lookup
    lib:idb:entry->library-name
    lib:idb:entry->library-id
  )
  (inline
    lib:idb:entry->library-name
    lib:import-set->import-set
  )
  (begin

(define (library? ast)
  (tagged-list? 'define-library ast))
 
;; Determine if a library exists for the given import set
(define (library-exists? import . ext)
  (file-exists?
    (lib:import->filename 
      (lib:import->library-name import)
      (if (null? ext) ".sld" (car ext)))))

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

;; Is import set just a library name?
(define (lib:import-set:library-name? import-set)
  (not
    (or (tagged-list? 'only import-set)
        (tagged-list? 'except import-set)
        (tagged-list? 'prefix import-set)
        (tagged-list? 'rename import-set))))

;; lib:import-set->import-set -> list -> list
;; Extract next import set from given input set
(define (lib:import-set->import-set import-set)
  (cadr import-set))

;; Convert an import-set to its corresponding library name.
;; These are not always the same thing, but each import-set
;; does reference a specific library.
(define (lib:import->library-name import)
  (cond
    ((or (tagged-list? 'only import)
         (tagged-list? 'except import)
         (tagged-list? 'prefix import)
         (tagged-list? 'rename import))
     (lib:import->library-name 
       (cadr import)))
    (else
     import)))

;; Convert name (as list of symbols) to a mangled string
(define (lib:name->string name)
  (apply string-append (map mangle (lib:import->library-name name))))

;; Convert name (as list of symbols) to a mangled string guaranteed to be unique
(define (lib:name->unique-string name)
  (foldl
    (lambda (s acc)
      (if (> (string-length acc) 0)
          (string-append acc "_" s)
          s))
    ""
    (map mangle (lib:import->library-name name))))

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

(define (lib:c-linker-options ast)
  (map
    (lambda (inc-lst)
      (cadr inc-lst))
    (filter
      (lambda (code)
        (tagged-list? 'c-linker-options code))
      (cddr ast))))

(define (lib:include-c-headers ast)
  (map
    (lambda (inc-lst)
      (cadr inc-lst))
    (filter
      (lambda (code)
        (tagged-list? 'include-c-header code))
      (cddr ast))))

(define (lib:inlines ast)
  (apply 
    append
    (map
      (lambda (inc-lst)
        (cdr inc-lst))
      (filter
        (lambda (code)
          (tagged-list? 'inline code))
        (cddr ast)))))

;; TODO: include-ci, cond-expand

;TODO: maybe just want a function that will take a define-library expression and expand any top-level cond-expand expressions.
;then just return all of that. the front-end can then call this function once to pre-process the library code before any further compilation.
;
;obviously also need to expand cond-expand in cases where the code reads sld files to track down library dependencies

;; Take given define-library expression and cond-expand all declarations
(define (lib:cond-expand expr expander)
  (let ((name (cadr expr))
        (decls (lib:cond-expand-decls (cddr expr) expander)))
    `(define-library ,name ,@decls)))

(define (lib:cond-expand-decls decls expander)
  (reverse
    (foldl 
      (lambda (d acc) 
        (cond
          ((tagged-list? 'cond-expand d)
           ;; Can have more than one ce expression, EG:
           ;; (cond-expand
           ;;  (cyclone
           ;;    (import ...)
           ;;    (export ...)
           ;;
           ;;  TODO: handle this properly
           (let* ((expr (expander d)))
             (cond
              ;; Special case, multiple sub-expressions
              ((and (pair? expr)
                    (lambda? (car expr))
                    (eq? '() (lambda->formals (car expr))))
               (append 
                 (reverse ;; Preserve order
                   (map form-ce-expr (lambda->exp (car expr))))
                 acc))
              (else
               (cons (form-ce-expr expr) acc)))))
          (else
            (cons d acc)) ))
      '() 
     decls)))

(define (form-ce-expr expr)
  (cond
   ((and (pair? expr)
         (not (member (car expr) 
                     '(import export c-linker-options include-c-header))))
    `(begin ,expr))
   (else
    expr)))

(define (lib:atom->string atom)
  (cond
    ((symbol? atom)
     (symbol->string atom))
    ((number? atom)
     (number->string atom))
    (else
     (error "Unexpected type in import set"))))

;; Resolve library filename given an import. 
;; Options:
;; - Extension, assumes ".sld" file extension if one is not specified.
;; - Append path, list of strings
;; - Prepend path, list of strings
(define (lib:import->filename import . opts)
  (let* ((file-ext 
          (if (null? opts)
              ".sld"
              (car opts)))
         (append-dirs
          (if (or (null? opts) (null? (cdr opts)))
              '()
              (cadr opts)))
         (prepend-dirs
          (if (or (null? opts) (null? (cdr opts)) (null? (cddr opts)))
              '()
              (caddr opts)))
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
           (substring filename* 1 (string-length filename*)))
         (dir (if (or (tagged-list? 'scheme import)
                      (tagged-list? 'srfi import)
                      (tagged-list? 'cyclone import))
                  (list (Cyc-installation-dir 'sld) "./")
                  (list "./"))))
    (call/cc
      (lambda (return)
        (for-each
          (lambda (path)
            (let ((f (string-append path "/" filename)))
              (if (file-exists? f) 
                  (return f))))
          (append prepend-dirs dir append-dirs))
        ;; Not found, just return base name
        (lib:check-system-path
          (if (> (length dir) 0)
              (string-append (car dir) "/" filename)
              filename))))
  ))

;; Get path to directory that contains the library
(define (lib:import->path import append-dirs prepend-dirs include)
  (let* ((import-path (reverse (cdr (reverse import))))
         (path
           (apply
             string-append
             (map 
               (lambda (i) 
                 (string-append (lib:atom->string i) "/"))
               import-path)))
         (filename
          (string-append path "" include))
         (dir (if (or (tagged-list? 'scheme import)
                      ;(tagged-list? 'srfi import)
                      (tagged-list? 'cyclone import)
                  )
                  (list "./" (Cyc-installation-dir 'sld))
                  (list "./"))))
    (call/cc
      (lambda (return)
        (for-each
          (lambda (path)
            (let ((f (string-append path "/" filename)))
              ;(write `(DEBUG ,path ,f ,(file-exists? f)))
              ;(newline)
              (if (file-exists? f) 
                  (return f))))
          (append prepend-dirs dir append-dirs))
        ;; Not found, just return base name
        (lib:check-system-path
          (if (> (length dir) 1)
              (string-append (cadr dir) "/" filename)
              (string-append (car dir) "/" filename)))))
  ))

;; string :: string
;;
;; Check the system path to see if the given library is present.
;; If so return the full path, otherwise give up and return filename.
;;
(define (lib:check-system-path filename)
  (let* ((env-dir (get-environment-variable "CYCLONE_LIBRARY_PATH"))
         (dir (if env-dir 
                  env-dir 
                  (Cyc-installation-dir 'sld)))
         (path (string-append dir "/" filename)))
    (if (file-exists? path)
        path
        filename)))

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
(define (lib:read-imports import append-dirs prepend-dirs expander)
  (let* ((lib-name (lib:import->library-name import))
         (dir (lib:import->filename lib-name ".sld" append-dirs prepend-dirs))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (lib* (if expander
                   (list (lib:cond-expand (car lib) expander))
                   lib))
         (imports (lib:imports (car lib*))))
    (close-input-port fp)
    imports))

;; Given a single import from an import-set, open the corresponding
;; library file and retrieve the library's includes
(define (lib:read-includes import append-dirs prepend-dirs)
  (let* ((lib-name (lib:import->library-name import))
         (dir (lib:import->filename lib-name ".sld" append-dirs prepend-dirs))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (includes (lib:includes (car lib))))
    (close-input-port fp)
    includes))

(define (lib:read-c-linker-options import append-dirs prepend-dirs)
  (let* ((lib-name (lib:import->library-name import))
         (dir (lib:import->filename lib-name ".sld" append-dirs prepend-dirs))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (options (lib:c-linker-options (car lib))))
    (close-input-port fp)
    (string-join options " ")))

(define (lib:get-all-c-linker-options imports append-dirs prepend-dirs)
  (string-join
    (map 
      (lambda (import)
        (lib:read-c-linker-options import append-dirs prepend-dirs))
      imports)
    " "))

;; Read export list for a given import
(define (lib:import->export-list import append-dirs prepend-dirs expander)
  (let* ((lib-name (lib:import->library-name import))
         (dir (string-append (lib:import->filename lib-name ".sld" append-dirs prepend-dirs)))
         (fp (open-input-file dir))
         (lib (read-all fp))
         (lib* (if expander
                   (list (lib:cond-expand (car lib) expander))
                   lib))
         (exports (lib:exports (car lib*))))
    (close-input-port fp)
    (lib:import-set/exports->imports import exports)))

;; Take an import set and the corresponding list of exports. Process all of the
;; import set directives (only, except, rename, prefix) and return a list of identifiers to import based on the export list.
;;
;; Any identifiers renamed in the export list will be returned as a pair
;; of the form (renamed-ident . original-ident)
;; 
(define (lib:import-set/exports->imports import-set exports)
  ;; Handle import set that contains another import set
  (unless (lib:import-set:library-name? import-set)
    (let ((result (lib:import-set/exports->imports
                    (lib:import-set->import-set import-set)
                    exports)))
      (set! exports result)))
  ;; Process the current import set
  (cond
    ((tagged-list? 'only import-set)
     ;; Filter to symbols from "only" that appear in export list
      (let ((only-syms (cddr import-set)))
        (filter
          (lambda (sym)
            (member
              (if (pair? sym) (car sym) sym)
              only-syms))
          exports)))
    ((tagged-list? 'except import-set)
     (let ((except-syms (cddr import-set)))
       (filter
         (lambda (sym)
           (not (member 
                  (if (pair? sym) (car sym) sym)
                  except-syms)))
         exports)))
    ((tagged-list? 'prefix import-set)
     ;; same as rename, but add given prefix to all exports
     (let* ((prefix (caddr import-set))
            (prestr (symbol->string prefix)))
       (map
         (lambda (e)
            (cons
              ;; Renamed identifier with prefix
              (string->symbol
                (string-append
                  prestr
                  (symbol->string 
                    (if (pair? e)
                        (car e)
                        e))))
              ;; Original identifier
              (if (pair? e)
                  (cdr e)
                  e)))
         exports)))
    ((tagged-list? 'rename import-set)
     (let ((renames (cddr import-set)))
       (map
         (lambda (e)
           (let ((rename (assoc 
                           (if (pair? e) (car e) e) 
                           renames)))
            (if rename
                (cons 
                  (cadr rename) ;; Renamed identifier
                  (if (pair? e) (cdr e) e) ;; Original identifier from library
                )
                e)))
         exports)))
    (else
      exports)))
;; Test cases for above:
;cyclone> (lib:import-set/exports->imports '(lib) '(a b c d e))
;(a b c d e)
;cyclone> (lib:import-set/exports->imports '(except (lib) a) '(a b c d e))
;(b c d e)
;cyclone> (lib:import-set/exports->imports '(rename (lib) (a a1) (d d1)) '(a b c d e))
;((a1 . a) b c (d1 . d) e)
;cyclone> (lib:import-set/exports->imports '(rename (rename (lib) (a a1) (d d1)) (d1 d2)) '(a b c d e))
;((a1 . a) b c (d2 . d) e)
;cyclone> (lib:import-set/exports->imports '(prefix (lib) my-) '(a b c d e))
;((my-a . a) (my-b . b) (my-c . c) (my-d . d) (my-e . e))
;cyclone> (lib:import-set/exports->imports '(only (prefix (lib) my-) my-b) '(a b c d e))
;
; (lib:import-set/exports->imports '(except (rename (lib) (a a1) (d d1)) d1 e) '(a b c d e))
;

;; Take a list of imports and resolve it to the imported vars
;(define (lib:resolve-imports imports)
; (apply
;   append
;   (map 
;     (lambda (import)
;       (lib:import->export-list import))
;     (map lib:list->import-set imports))))

;; Take a list of imports and create a "database" from them
;; consisting of maps between each exported identifier and the
;; library that imports that identifier. 
;;
;; TODO: Raise an exception if the same identifier is exported 
;; from more than one library???
;;
;; TODO: convert this to use a hashtable. Initially a-lists
;; will be used to prove out the concept, but this is inefficient
(define (lib:imports->idb imports append-dirs prepend-dirs expander)
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
           (lib:import->export-list import-set append-dirs prepend-dirs expander))))
     (map lib:list->import-set imports))))

;; Convert from the import DB to a list of identifiers that are imported.
;; EG: '((call/cc . (scheme base))) ==> '(call/cc)
(define (lib:idb:ids db)
  (foldr 
    (lambda (i is) 
      (let ((id (if (pair? (car i)) (caar i) (car i))))
        (cons id is)))
   '() 
    db))

;; Retrieve entry in the given idb database for the given identifier
(define (lib:idb:lookup db identifier)
  (call/cc 
    (lambda (return)
      (for-each
        (lambda (entry)
          (cond
            ;; Normal identifier, no renaming
            ((equal? identifier (car entry)) 
             (return entry))
            ;; Identifier was renamed by an import set
            ((and (pair? (car entry))
                  (equal? identifier (caar entry)))
             (return entry))
            ;; Keep going
            (else #f)))
        db)
      (return #f))))

;; Take an idb entry and find the library that imported it
(define (lib:idb:entry->library-name entry)
  (if entry
      (cdr entry)
      #f))

;; Take an idb entry and find the original identifier for it,
;; that is part of the library definition.
(define (lib:idb:entry->library-id entry)
  (if (pair? entry)
      (cond
        ;; ID was renamed by an import set
        ((pair? (car entry))
         (cdar entry))
        (else
          (car entry)))
      #f))

(define (lib:import->metalist import append-dirs prepend-dirs)
  (let* ((lib-name (lib:import->library-name import))
         (file (lib:import->filename lib-name ".meta" append-dirs prepend-dirs))
         (fp #f)
         (result '()))
    (cond
      ((file-exists? file)
       (set! fp (open-input-file file))
       (set! result (car (read-all fp)))
       (close-input-port fp)))
    result))

(define (lib:resolve-meta imports append-dirs prepend-dirs)
 (apply
   append
   (map 
     (lambda (import)
       (lib:import->metalist import append-dirs prepend-dirs))
     imports)))

;; Given an import set, get all dependant import names that are required
;; The list of deps is intended to be returned in order, such that the
;; libraries can be initialized properly in sequence.
(define (lib:get-all-import-deps imports append-dirs prepend-dirs expander)
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
                    (let* ((deps (lib:read-imports import-set append-dirs prepend-dirs expander))
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
