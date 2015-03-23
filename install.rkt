#lang racket

(define local-collects-path
  (first (current-library-collection-paths)))

(define local-install-path  
  (build-path local-collects-path "vigracket"))

;; 1. Create local collects directory if not already existing
(when (not (directory-exists? local-collects-path))
  (system (string-append "mkdir -p " (path->string local-collects-path))))

;; 2. If vigracket-collects dir extist, delete it.
(when (directory-exists? local-install-path)
  (delete-directory local-install-path))

;; 3. copy the installation contents to the local/collects/vigracket directory
(copy-directory/files (current-directory) local-install-path)

;; Load dll under windows, dylib under MacOS
(define dylib-file
    (cond ((equal? (system-type 'os) 'windows) "vigra_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libvigra_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libvigra_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define dylib-path (build-path local-install-path dylib-file))

;; For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string local-install-path) ";" (getenv "PATH"))))

;; Stuff needed to compile the c-bindings if necessary...
(define base_login_script "~/.profile")
(define vigra_c-path (build-path local-install-path "vigra_c"))

(define login_script (if (file-exists? base_login_script)
                         base_login_script
                         (path->string (build-path vigra_c-path "fallback.profile"))))

(define login_cmd (string-append "source " login_script))
(define (system-env arg) (system (string-append login_cmd " && " arg)))

(define (vigra-installed?) 
  (display "Searching for vigra using 'vigra-config --version': ")
  (system-env "vigra-config --version"))

;; For windows, we need to find out, which architecture DrRacket is built
(require (only-in ffi/unsafe ctype-sizeof _pointer))
(define racket-bits (* 8 (ctype-sizeof _pointer)))

; The compilation routine (at least for macosx and unix)
(if (or (equal? (system-type 'os) 'macosx)
        (equal? (system-type 'os) 'unix))
    (if (vigra-installed?)
        ;;VIGRA is found! Try to compile vigra_c bindings
        (begin 
          (display "-------------- BUILDING VIGRA-C-WRAPPER FOR COMPUTER VISION AND IMAGE PROCESSING TASKS --------------")
          (newline)
          (current-directory vigra_c-path)
          (if (system-env (string-append " make " (symbol->string (system-type 'os)) (number->string racket-bits))) ; "make macosx32",  "make macosx64", "make unix32"  or "make unix64"
              (begin
                (copy-file (build-path (current-directory) "bin"   dylib-file) dylib-path #t)
                #t)
              (error "making the vigra_c lib failed, although vigra seems to be installed")))
        (error "Vigra is not found. Please check if the prefix path is set correctly in ~/.profile environment file!"))
    ;;For windows
    (if (equal? (system-type 'os) 'windows)
        (let ((bindir     (build-path vigra_c-path "bin" (string-append "win"(number->string racket-bits)))))
          (begin
            (system (string-append "copy " (path->string bindir) "\\*.dll " (path->string local-install-path)))
            (system (string-append "copy " (path->string local-install-path) "\\zlib.dll " (path->string local-install-path)  "\\zlibwapi.dll"))
            #t))
        (error "Only Mac OS X, Unix are supported for auto build of vigra_c!")))
