#lang racket

;; Module constants
(define vigracket-path  (collection-path "vigracket"))
(define vigracket-version "1.0.0")

;; For windows, we need to find out, which architecture DrRacket is built
(require (only-in ffi/unsafe ctype-sizeof _pointer))
(define racket-bits (* 8 (ctype-sizeof _pointer)))

;; Load dll under windows, dylib under MacOS
(define dylib-file
    (cond ((equal? (system-type 'os) 'windows) "vigra_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libvigra_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libvigra_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define vigracket-dylib-path (build-path vigracket-path dylib-file))

;; Stuff needed to compile the c-bindings if necessary...
(define base_login_script "~/.profile")
(define vigra_c-path (build-path vigracket-path "vigra_c"))

(define login_script (if (file-exists? base_login_script)
                         base_login_script
                         (path->string (build-path vigra_c-path "fallback.profile"))))

(define login_cmd (string-append "source " login_script))
(define (system-env arg) (system (string-append login_cmd " && " arg)))

(define (vigra-installed?) 
  (display "Searching for vigra using 'vigra-config --version': ")
  (system-env "vigra-config --version"))

; The compilation routine (at least for macosx and unix)
(define (build-vigra_c)
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
                  (copy-file (build-path (current-directory) "bin" dylib-file) vigracket-dylib-path)
                  #t)
                (error "making the vigra_c lib failed, although vigra seems to be installed")))
          (error "Vigra is not found. Please check if the prefix path is set correctly in ~/.profile environment file!"))
      ;;For windows
      (if (equal? (system-type 'os) 'windows)
          (let ((bindir     (build-path vigra_c-path "bin" (string-append "win"(number->string racket-bits)))))
             (begin
               (system (string-append "copy " (path->string bindir) "\\*.dll " (path->string vigracket-path)))
               (system (string-append "copy " (path->string vigracket-path) "\\zlib.dll " (path->string vigracket-path)  "\\zlibwapi.dll"))
               #t))
          (error "Only Mac OS X, Unix are supported for auto build of vigra_c!"))))

;;Enable Auto-Build of the vigra-c-lib if not already present!
(when (not (file-exists? vigracket-dylib-path)) 
   (build-vigra_c))
 
;; For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string vigracket-path) ";" (getenv "PATH"))))

(provide vigracket-path
         vigracket-version
         vigracket-dylib-path)

