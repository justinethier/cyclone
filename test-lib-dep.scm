(import 
  (scheme base)
  (scheme write)
  (scheme cyclone libraries)
)


(define append-dirs '())
(define prepend-dirs '())
;(define lib-dep '(tmp))
(define lib-dep '(scheme cyclone common2))

(let* ((sld-file (lib:import->filename lib-dep ".sld" append-dirs prepend-dirs))
       (obj-file (lib:import->filename lib-dep ".o" append-dirs prepend-dirs))
      )
  (write (list 
          (file-mtime sld-file)
          (file-mtime obj-file)
          (recompile? lib-dep)
          )))

(define (recompile? lib-dep)
  (let* ((sld-file (lib:import->filename lib-dep ".sld" append-dirs prepend-dirs))
         (obj-file (lib:import->filename lib-dep ".o" append-dirs prepend-dirs)) ;; TODO: update base name??
        )
    (> (file-mtime sld-file)
       (file-mtime obj-file)))) ;; Is obj file out of date??

(define-c file-mtime
  "(void *data, int argc, closure _, object k, object filename)"
  " make_double(box, 0.0);
    Cyc_check_str(data, filename);
    double_value(&box) = Cyc_file_last_modified_time(string_str(filename));
    return_closcall1(data, k, &box); ")
