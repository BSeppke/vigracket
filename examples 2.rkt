#lang racket

(require racket/flonum)

;Always the first step: load the VigRACKET lib
(require vigracket)

(display "loading sketch-image")(newline)
(define img1 (loadimage  (build-path vigracket-path "images/hough-test.tif")))

(define hough-lines (houghtransform-lines img1 36 50))
(image->list hough-lines)


(display "loading shoes-image")(newline)
(define img2 (loadimage  (build-path vigracket-path "images/shoe.png")))

(define hough-circles (houghtransform-circles img2 5 30 0.67))
(image->list hough-circles)
