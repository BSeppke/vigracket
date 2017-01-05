#lang racket

(define local-collects-path
  (first (current-library-collection-paths)))

(define local-install-path  
  (build-path local-collects-path "vigracket"))

(define (prepare-installation)
  ;; 1. Create local collects directory if not already existing
  (when (not (directory-exists? local-collects-path))
    (make-directory* local-collects-path))
  ;; 2. If vigracket-collects dir extist, delete it.
  (when (directory-exists? local-install-path)
    (delete-directory/files local-install-path))
  ;; 3. copy the installation contents to the local/collects/vigracket directory
  (copy-directory/files (current-directory) local-install-path)
  ;; 4. For Windows: Add the dll directory to the systems path:
  (when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string local-install-path) ";" (getenv "PATH"))))
  ;; 5. For Mac OS X: Add MacPorts dir to Path, so that vigra-config etc. can be found
  (when (equal? (system-type 'os) 'macosx)
    (putenv "PATH" (string-append "/opt/local/bin" ":" (getenv "PATH")))))

(define dylib-file
  (cond ((equal? (system-type 'os) 'windows) "vigra_c.dll")
        ((equal? (system-type 'os) 'macosx)  "libvigra_c.dylib")
        ((equal? (system-type 'os) 'unix)    "libvigra_c.so")
        (else (error "Only macosx, windows and unix are supported"))))

(define dylib-path (build-path local-install-path dylib-file))
(define vigra_c-path (build-path local-install-path "vigra_c"))

(define (vigra-version)
  (let* ((version_string
          (string-trim (with-output-to-string (lambda ()
                                                (system "vigra-config --version"))))))
    (if (not (equal? version_string ""))
        (begin (display version_string)(newline)
               (map string->number (string-split version_string ".")))
        '())))

(define (vigra-installed?)
  (display "Searching for vigra-config tool")
  (if (find-executable-path "vigra-config")
      (let ((foo (display "Searching for vigra >= 1.11.0 using 'vigra-config --version': "))
            (version (vigra-version)))
        (if (empty? version)
            #f
            (or (and (= (first version) 1) (>= (second version) 11))
                (> (first version) 1))))
      #f))

(require (only-in ffi/unsafe ctype-sizeof _pointer))
(define racket-bits (* 8 (ctype-sizeof _pointer)))
(define cmake-flags (if (= racket-bits 32)
                        "-DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS=-m32 -DCMAKE_C_FLAGS=-m32"
                        "-DCMAKE_BUILD_TYPE=Release"))

;; The compilation routine (at least for macosx and unix)
(define (check-install-vigracket)
  (prepare-installation)
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
                  (copy-file (build-path (current-directory) "bin"   dylib-file) dylib-path #t)
                  #t)
                (error "making the vigra_c lib failed, although vigra seems to be installed")))
          (error "Vigra is not found. Please check if the prefix path is set correctly in ~/.profile environment file!"))
      ;;For windows: Just copy the correct binaries (no system called needed herein
      (let ((bindir     (build-path vigra_c-path "bin" (string-append "win"(number->string racket-bits)))))
        (if (equal? (system-type 'os) 'windows)
            (let* ((binaries   (find-files (compose (curry equal?  #".dll") path-get-extension)  bindir))
                   (result     (map (lambda (f) (copy-file f (path->string (build-path local-install-path (file-name-from-path f))) #t)) binaries)))
              (if (foldl (lambda (x y) (and x y)) #t result)
                  #t
                  (error (string-append "Copying of vigra_c's binaries from " (path->string bindir) " to "  (path->string local-install-path) " failed !"))))
            (error "Only windows, Mac OS X and Unix are supported!")))))

(check-install-vigracket)