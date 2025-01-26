;; cyclone.scm
(use-modules
 (gnu packages)
 (gnu packages multiprecision)
 ((guix licenses)
  #:select (gpl2 gpl2+ lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3+ asl2.0
                 bsd-0 bsd-3 cc-by-sa4.0 non-copyleft expat
                 public-domain))
 (guix gexp)
 (guix packages)
 (guix download)
 (guix git-download)
 (guix utils)
 (guix build-system gnu)
 (gnu packages c))

(define-public cyclone
  (package
    (name "cyclone")
    (version "0.36.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/justinethier/cyclone-bootstrap")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0fv0mnrn5shbx77383f4mbkvc4i9yyj1bjm3dfyhipnaqapbhqpi"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list (string-append
                            "CC=" #$(this-package-input "gcc-toolchain")
                            "/bin/gcc")
                           (string-append "PREFIX=" #$output)
                           (string-append "COMP_INCDIRS=-I$(PREFIX)/include -I"
                                          #$(this-package-input "gcc-toolchain")
                                          "/include")
                           (string-append
                            "COMP_LIBDIRS=-L$(PREFIX)/lib "
                            "-Wl,-rpath=" #$(this-package-input "ck") "/lib "
                            "-L" #$(this-package-input "ck") "/lib "
                            "-Wl,-rpath=" #$(this-package-input "libtommath")
                            "/lib "
                            "-L" #$(this-package-input "libtommath") "/lib "
                            "-Wl,-rpath="
                            #$(this-package-input "gcc-toolchain") "/lib "
                            "-L" #$(this-package-input "gcc-toolchain")
                            "/lib"))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure) ; no configure script
                   (add-before 'build 'replace-cyclonebn
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("-lcyclonebn")
                          "-ltommath")
                         (("^[$][(]CYC_BN_LIB[)] :")
                          "dont-build-cyclonebn :")
                         (("^	[$][(]INSTALL[)] .* [$][(]CYC_BN_LIB[)].*$")
                          "#dont-install-cyclonebn\n")
                         (("[$][(]CYC_BN_LIB[)]")
                          ""))
                       (substitute* "Makefile.config"
                         (("-lcyclonebn")
                          "-ltommath"))))
                   (add-after 'install 'wrap
                     (lambda _
                       (wrap-program (string-append #$output "/bin/cyclone")
                         `("LIBRARY_PATH" ":" prefix
                           ,(list (string-append
                                   #$(this-package-input "gcc-toolchain")
                                   "/lib")))))))))
    (inputs (list ck libtommath (module-ref (resolve-interface
                                             '(gnu packages commencement))
                                            'gcc-toolchain)))
    (home-page "https://justinethier.github.io/cyclone/")
    (synopsis "R7RS Scheme to C compiler")
    (description
     "Cyclone Scheme is a R7RS Scheme-to-C compiler that uses a variant of
Cheney on the MTA to implement full tail recursion, continuations, and
generational garbage collection.  It also includes the Winds package manager
for installing Cyclone libraries.")
    (license expat)))

cyclone
