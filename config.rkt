#lang racket

(require ffi/unsafe)

;; Module constants
(define vigracket-path  (collection-path "vigracket"))
(define vigracket-version "1.4.0")

;; Load dll under windows, dylib under MacOS
(define vigracket-dylib-file
  (cond ((equal? (system-type 'os) 'windows) "vigra_c.dll")
        ((equal? (system-type 'os) 'macosx)  "libvigra_c.dylib")
        ((equal? (system-type 'os) 'unix)    "libvigra_c.so")
        (else (error "Only macosx, windows and unix are supported"))))

(define vigracket-dylib-path (build-path vigracket-path vigracket-dylib-file))	

;; The compilation routine (at least for macosx and unix)
(define (rebuild-vigracket)
  (define vigra_c-path (build-path vigracket-path "vigra_c"))
  (define (vigra-version)
    (let* ((version_string
            (string-trim (with-output-to-string (lambda ()
                                                  (system "vigra-config --version"))))))
      (if (not (equal? version_string ""))
          (begin (display version_string)(newline)
                 (map string->number (string-split version_string ".")))
          '())))
  (define (vigra-installed?)
    (display "Need to rebuild vigra_c lib!\nSearching for vigra-config tool\n")
    (if (find-executable-path "vigra-config")
        (let ((foo (display "Searching for vigra >= 1.11.0 using 'vigra-config --version': "))
              (version (vigra-version)))
          (if (empty? version)
              #f
              (or (and (= (first version) 1) (>= (second version) 11))
                  (> (first version) 1))))
        #f))
  (define racket-bits (* 8 (ctype-sizeof _pointer)))
  (define cmake-flags (if (= racket-bits 32)
                          "-DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS=-m32 -DCMAKE_C_FLAGS=-m32"
                          "-DCMAKE_BUILD_TYPE=Release"))
  (begin
    (if (or (equal? (system-type 'os) 'macosx)
            (equal? (system-type 'os) 'unix))
        (if (vigra-installed?)
            ;;VIGRA is found! Try to compile vigra_c bindings
            (begin 
              (display "-------------- BUILDING VIGRA-C-WRAPPER FOR COMPUTER VISION AND IMAGE PROCESSING TASKS --------------")
              (newline)
              (current-directory vigra_c-path)
              (if (system (string-append "mkdir build && cd build && cmake " cmake-flags " .. && make && cd .. && rm -rf ./build"))
                  (begin
                    (copy-file (build-path (current-directory) "bin"  vigracket-dylib-file) vigracket-dylib-path #t)
                    #t)
                  (error "making the vigra_c lib failed, although vigra seems to be installed")))
            (error "Vigra is not found. Please check if the vigra-config tool is in the environment path!"))
        ;;For windows: Just copy the correct binaries (no system called needed herein
        (let ((bindir     (build-path vigra_c-path "bin" (string-append "win"(number->string racket-bits)))))
          (if (equal? (system-type 'os) 'windows)
              (let* ((binaries   (find-files (compose (curry equal?  #".dll") path-get-extension)  bindir))
                     (result     (map (lambda (f) (copy-file f (path->string (build-path vigracket-path (file-name-from-path f))) #t)) binaries)))
                (if (foldl (lambda (x y) (and x y)) #t result)
                    #t
                    (error (string-append "Copying of vigra_c's binaries from " (path->string bindir) " to "  (path->string vigracket-path) " failed !"))))
              (error "Only windows, Mac OS X and Unix are supported!"))))
    ;;If it still fails loading -> give up and inform the user
    (void (ffi-lib vigracket-dylib-path #:fail (lambda() (error "The vigra_c (still) cannot be loaded after one rebuild phase"))))))


;; For Windows: Add the dll directory to the systems path:
(void (when (equal? (system-type 'os) 'windows)
        (putenv "PATH" (string-append (path->string vigracket-path) ";" (getenv "PATH")))))

;; For Mac OS X: Add the MacPorts directory to the systems path
(void (when (equal? (system-type 'os) 'macosx)
        (putenv "PATH" (string-append "/opt/local/bin" ":" (getenv "PATH")))))

;;Try to load the dylib: If this fails - try to rebuild it!
(void (ffi-lib vigracket-dylib-path #:fail rebuild-vigracket))


(provide vigracket-path
         vigracket-version
         vigracket-dylib-file
         vigracket-dylib-path)

