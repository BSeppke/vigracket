#lang racket

(define local-collects-path
  (first (current-library-collection-paths)))

(define local-install-path  
  (build-path local-collects-path "vigracket"))

;; 1. Create local collects directory if not already existing
(when (not (directory-exists? local-collects-path))
  (make-directory* local-collects-path))

;; 2. If vigracket-collects dir extist, delete it.
(when (directory-exists? local-install-path)
  (delete-directory/files local-install-path))

;; 3. copy the installation contents to the local/collects/vigracket directory
(copy-directory/files (current-directory) local-install-path)