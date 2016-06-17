#lang racket

(require ffi/unsafe)

;; Module constants
(define vigracket-path  (collection-path "vigracket"))
(define vigracket-version "1.1.0")

;; Load dll under windows, dylib under MacOS
(define vigracket-dylib-file
    (cond ((equal? (system-type 'os) 'windows) "vigra_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libvigra_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libvigra_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define vigracket-dylib-path (build-path vigracket-path vigracket-dylib-file))

;; For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string vigracket-path) ";" (getenv "PATH"))))

(provide vigracket-path
         vigracket-version
         vigracket-dylib-file
         vigracket-dylib-path)

