#lang racket

;Always the first step: load the VigRACKET lib

(require vigracket)

(define magnitude 
  (lambda args 
    (sqrt (apply + (map (lambda (x) (* x x)) args)))))

(define save-path (find-system-path 'temp-dir))
(make-directory* (build-path save-path "images"))

(display "loading blox-image")(newline)
 (define img (loadimage  (build-path vigracket-path "images/blox.gif")))

(display "performance test gaussian smoothing")(newline)
(display "vigra-method [in ms]:")(newline)
(time (gsmooth img 0.3))


(display "performing watershed transform on resized gradient image")(newline)
 (define  img2 (regionimagetocrackedgeimage
                (labelimage
                 (watersheds
                  (ggradient 
                   (resizeimage img (* 2 (image-width img))(* 2 (image-height img)) 4)
                   1.0)))
                0.0))

(display "performing fft on image")(newline)
(define  img3 (fouriertransform (loadimage (build-path vigracket-path "images/rect.gif"))))
(define  img3magnitude (image-map magnitude  (first img3) (second img3)))

(display "testing rotation and reflection functions on image")(newline)
(define  img4 (reflectimage img 3))
(define  img5 (rotateimage img 15.0 3))


(display "testing affine transformation on image")(newline)
(define theta (/(* 15 3.14157) 180))

(define rotmat (make-matrix 3 3 0.0))
(matrix-set! rotmat 0 0 (cos theta))
(matrix-set! rotmat 1 1 (cos theta))
(matrix-set! rotmat 0 1 (* -1 (sin theta)))
(matrix-set! rotmat 1 0 (sin theta))
(matrix-set! rotmat 2 2 1.0)

(define t1mat (make-matrix 3 3 0.0))
(matrix-set! t1mat 0 0 1.0)
(matrix-set! t1mat 1 1 1.0)
(matrix-set! t1mat 2 2 1.0)
(matrix-set! t1mat 0 2 (/ (image-width img) -2.0))
(matrix-set! t1mat 1 2 (/ (image-width img) -2.0))

(define t2mat (make-matrix 3 3 0.0))
(matrix-set! t2mat 0 0 1.0)
(matrix-set! t2mat 1 1 1.0)
(matrix-set! t2mat 2 2 1.0)
(matrix-set! t2mat 0 2 (/ (image-width img) 2.0))
(matrix-set! t2mat 1 2 (/ (image-width img) 2.0))

(define  img6 (affinewarpimage img (matrix-mult t1mat (matrix-mult rotmat t2mat)) 3))

(display "performing distance transform on canny edges of image")(newline)
 (define  img7 (distancetransform
		  (cannyedgeimage img 1.8 0.1 100.0)
		  0.0 2))

(display "testing difference of exponential edge detection on image")(newline)
(define  img8 (differenceofexponentialedgeimage img 1.8 0.5 100.0))

(display "testing nearly all filters")(newline)
(define  img9 (gsmooth img 3.0))
(define  img10 (laplacianofgaussian img 3.0))
(define  img11 (gsharpening img 0.5 3.0))
(define  img12 (sharpening img 3.0))
(define  img13 (nonlineardiffusion img 0.1 2.0))

(display "saving resulting images")(newline)
(saveimage img2   (build-path save-path "images/blox-relabeled-watersheds-on-resized-gradient-image.png"))

(saveimage img3magnitude (build-path save-path "images/rect-fft-magnitude.png"))
(saveimage (image-map sqrt img3magnitude) (build-path save-path "images/rect-fft-sqrt-magnitude.png"))

(saveimage img4   (build-path save-path "images/blox-reflected-both.png"))
(saveimage img5   (build-path save-path "images/blox-rotated-15deg.png"))
(saveimage img6   (build-path save-path "images/blox-aff-rotated-15deg.png"))
(saveimage img7   (build-path save-path "images/blox-disttransform-on-canny.png"))
(saveimage img8   (build-path save-path "images/blox-diff_of_exp.png"))
(saveimage img9   (build-path save-path "images/blox-gsmooth-3.0.png"))
(saveimage img10  (build-path save-path "images/blox-log-3.0.png"))
(saveimage img11  (build-path save-path "images/blox-gsharpening-0.5-3.0.png"))
(saveimage img12  (build-path save-path "images/blox-sharpening-3.0.png"))
(saveimage img13  (build-path save-path "images/blox-nonlineardiffusion-0.1-2.0.png"))

(show-image img5)
(show-image img6 "title: img6")

;Test color images
(define img1 (loadimage (build-path vigracket-path "images/lenna_face.png")))

(show-image  (image->red img1) "Lennas red channel")
(show-image  (image->green img1) "Lennas green channel")
(show-image  (image->blue img1) "Lennas blue channel")
(show-image  img1 "Lenna in RGB")

; Tensor tests
(define img1_st  (structuretensor img1 1.0 4.0))

; boundary tensor
(define img1_bt  (boundarytensor img1 1.0))

; boundary tensor without 0 order parts
(define img1_bt1  (boundarytensor1 img1 1.0))

;;tensor to eigen repr.
(define img1_st_te (tensoreigenrepresentation img1_st))

;tensor trace                
(define img1_st_tt (tensortrace img1_st))
(show-image  img1_st_tt "StructureTensor for Lenna    -    trace")

;tensor to edge corner
(define img1_st_ec (tensortoedgecorner img1_st))
(show-image (first img1_st_ec)  "StructureTensor for Lenna    -    edgeness")
(show-image (third img1_st_ec)  "StructureTensor for Lenna    -    cornerness")

;tensor to hourglass-filtered tensor
(define img1_st_hg (hourglassfilter img1_st 1.0 1.0))

(show-image (convolveimage img (list->matrix '((0.0 0.125 0.0) 
                                               (0.125  0.5 0.125) 
                                               (0.0 0.125 0.0)))) 
            "Approx. Gauss Convolution")

(show-image (convolveimage img (list->matrix '((0.1111 0.1111 0.1111) 
                                               (0.1111  0.1111 0.1111) 
                                               (0.1111 0.1111 0.1111)))) 
            "Box mean convolution")

(show-image (separableconvolveimage img1 
                                    (list->matrix '((0.3333) (0.3333) (0.3333))) 
                                    (list->matrix '((0.1 0.1 0.1 0.1 0.1 0.0 0.1 0.1 0.1 0.1 0.1))) )
            "Box mean separable convolution")