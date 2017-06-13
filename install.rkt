#lang racket

(define local-collects-path
  (first (current-library-collection-paths)))

(define local-install-path  
  (build-path local-collects-path "vigracket"))


(define (show-notes)
  (display (format "vigracket has been successfully installed in ~a." local-install-path))(newline)
  (display "Use it by typing (require vigracket) or run the examples (provided in examples.rkt)")(newline)
  (cond ((equal? (system-type 'os) 'windows)
         (begin
           (display "=== Important notes for Windows ===")(newline)
           (display "Please make sure to install the MS Visual C++ 2015 runtimes first!")(newline)
           (display "You may get them from: https://www.microsoft.com/en-us/download/details.aspx?id=48145")))         
        ((equal? (system-type 'os) 'macosx)
         (begin
           (display "=== Important notes for Max OS X ===")(newline)
           (display "Please install the MacPorts port system first! You get it from: https://www.macports.org")(newline)
           (display "After the installation of MacPorts, install CMake and vigra by typing:")(newline)
           (display "    sudo port install cmake")(newline)
           (display "    sudo port install vigra")(newline)
           (display "The wrapper library vigra_c will be build on the first load of the vigracket module!")))   
        ((equal? (system-type 'os) 'unix)
         (begin
           (display "=== Important notes for Linux ===")(newline)
           (display "Please make sure to install the vigra (version >= 1.11.0) first!")(newline)
           (display "You may get the latest version of the vigra from github using:")(newline)
           (display "    git clone https://github.com/ukoethe/vigra")(newline)
           (display "The wrapper library vigra_c will be build on the first load of the vigracket module!")))))

(define (install-vigracket)
  (begin
    ;; 1. Create local collects directory if not already existing
    (when (not (directory-exists? local-collects-path))
      (make-directory* local-collects-path))
    ;; 2. If vigracket-collects dir extist, delete it.
    (when (directory-exists? local-install-path)
      (delete-directory/files local-install-path))
    ;; 3. copy the installation contents to the local/collects/vigracket directory
    (copy-directory/files (current-directory) local-install-path)
    ;; 4. Tell the user about the status of the installation:
    (show-notes)))

;;Trigger the installation
(if (member (system-type 'os) '(windows macosx unix))
    (install-vigracket)
    (error "Sorry, but vigracket only supports Windows, Mac OS X or Linux"))
    