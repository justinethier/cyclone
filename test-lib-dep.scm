(import 
  (scheme base)
  (scheme file)
  (scheme write)
  (scheme cyclone libraries)
  (scheme cyclone pretty-print)
)


(define append-dirs '())
(define prepend-dirs '())
(define lib-dep '(scheme cyclone common2))

(pretty-print (list
  (list '(tmp) (recompile? '(tmp)))
  (list '(scheme cyclone common) (recompile? '(scheme cyclone common)))
  (list lib-dep (recompile? lib-dep))
))

;; recompile? :: [symbol] -> boolean
;; Do we need to recompile given library?
(define (recompile? lib-dep)
  (let* ((sld-file (lib:import->filename lib-dep ".sld" append-dirs prepend-dirs))
         (base (basename sld-file ".sld"))
         (obj-file (string-append base ".o"))
         (sys-dir (Cyc-installation-dir 'sld))
        )
    (and
      (not (in-subdir? sys-dir sld-file)) ;; Never try to recompile installed libraries
      (or
        (not (file-exists? obj-file)) ;; No obj file, must rebuild
        (> (file-mtime sld-file)
           (file-mtime obj-file)))))) ;; obj file out of date

;; Is "path" under given subdirectory "dir"?
(define (in-subdir? dir path)
  (and (>= (string-length path)
           (string-length dir))
       (equal? dir (substring path 0 (string-length dir)))))

(define (basename filename ext)
  (let* ((len (string-length filename))
         (ext-len (string-length ext)))
    (substring filename 0 (- len ext-len))))

(define-c file-mtime
  "(void *data, int argc, closure _, object k, object filename)"
  " make_double(box, 0.0);
    Cyc_check_str(data, filename);
    double_value(&box) = Cyc_file_last_modified_time(string_str(filename));
    return_closcall1(data, k, &box); ")
