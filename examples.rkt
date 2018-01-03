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

(display "testing conversion racket<->vigracket")(newline)
(define img_bitmap_img (bitmap->image (image->bitmap img)))

(display "testing subimage and correlation facilities")(newline)
(define img_cut (subimage img 100 50 151 101)) ;;Mask needs to have odd size!

(define fcc_res (fastcrosscorrelation img img_cut))
(define fncc_res (fastnormalizedcrosscorrelation img img_cut))
(define pos_matches (localmaxima fncc_res))
(define neg_matches (localminima fncc_res))


(display "test gaussian smoothing")(newline)
(time (gsmooth img 0.3))

(display "test DoG approx. as LoG (127 == zero)")(newline)
(show-image (image-map (lambda (x) (+ 127 x)) (image-map -  (gsmooth img 2) (gsmooth img 1))))

(display "performing watershed transform on resized gradient image")(newline)
(define  img2a (regionimagetocrackedgeimage
                (labelimage
                 (watersheds-uf
                  (ggradient 
                   (resizeimage img (* 2 (image-width img))(* 2 (image-height img)) 4)
                   1)))
                0))
(define  img2b (regionimagetocrackedgeimage
                (labelimage
                 (watersheds-rg
                  (ggradient 
                   (resizeimage img (* 2 (image-width img))(* 2 (image-height img)) 4)
                   1)))
                0))

(display "performing fft on image")(newline)
(define  img_rect (loadimage (build-path vigracket-path "images/rect.png")))
(define  img3 (fouriertransform (image->alpha img_rect)))
(define  img3magnitude (image-map magnitude  (first img3) (second img3)))

(define  img3ifft (fouriertransforminverse img3))
(define  img3small_ifft (fouriertransforminverse (map (lambda (img) (subimage img 20 20 (- (image-width img) 20) (- (image-height img) 20)))
                                                         img3)))

(display "testing rotation and reflection functions on image")(newline)
(define  img4 (reflectimage img 3))
(define  img5 (clipimage (rotateimage img 15 3) 0 255))


(display "testing affine transformation on image")(newline)
(define theta (/(* 15 3.14157) 180))

(define rotmat (make-matrix 3 3 0.0))
(matrix-set! rotmat 0 0 (cos theta))
(matrix-set! rotmat 0 1 (* -1 (sin theta)))
(matrix-set! rotmat 1 0 (sin theta))
(matrix-set! rotmat 1 1 (cos theta))
(matrix-set! rotmat 2 2 1.0)

(define t1mat (make-matrix 3 3 0.0))
(matrix-set! t1mat 0 0 1.0)
(matrix-set! t1mat 1 1 1.0)
(matrix-set! t1mat 2 2 1.0)
(matrix-set! t1mat 0 2 (/ (image-width img)  -2.0))
(matrix-set! t1mat 1 2 (/ (image-height img) -2.0))

(define t2mat (make-matrix 3 3 0.0))
(matrix-set! t2mat 0 0 1.0)
(matrix-set! t2mat 1 1 1.0)
(matrix-set! t2mat 2 2 1.0)
(matrix-set! t2mat 0 2 (/ (image-width img)  2.0))
(matrix-set! t2mat 1 2 (/ (image-height img) 2.0))

(define tmat (matrix-mult t1mat (matrix-mult rotmat t2mat)))

(define  img5aff (clipimage (affinewarpimage img tmat 3) 0 255))

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
(define  img13 (medianfilter img 3 3))
(define  img14 (nonlineardiffusion img 0.1 2.0))

(display "testing splineimageview facilities")(newline)

(define pos_x  34.23)
(define pos_y 123.23)
(define siv (create-splineimageview img 2))
	(splineimageview-value siv pos_x pos_y)
	(splineimageview-dx   siv pos_x pos_y)
	(splineimageview-dy   siv pos_x pos_y)
	(splineimageview-dxx  siv pos_x pos_y)
	(splineimageview-dxy  siv pos_x pos_y)
	(splineimageview-dyy  siv pos_x pos_y)
	(splineimageview-dx3  siv pos_x pos_y)
	(splineimageview-dxxy siv pos_x pos_y)
	(splineimageview-dxyy siv pos_x pos_y)
	(splineimageview-dy3  siv pos_x pos_y)
	(splineimageview-g2   siv pos_x pos_y)
	(splineimageview-g2x  siv pos_x pos_y)
	(splineimageview-g2y  siv pos_x pos_y)
	(splineimageview-g2xx siv pos_x pos_y)
	(splineimageview-g2xy siv pos_x pos_y)
	(splineimageview-g2yy siv pos_x pos_y)
(void (delete-splineimageview siv))

(show-image img5)
(show-image img5aff "title: img5aff")

;Test color images
(define img1 (loadimage (build-path vigracket-path "images/lenna_face.png")))

(show-image  (image->red img1) "Lennas red channel")
(show-image  (image->green img1) "Lennas green channel")
(show-image  (image->blue img1) "Lennas blue channel")
(show-image  img1 "Lenna in RGB")
(show-image  (paddimage img1 10 20 30 40 '(255.0 0.0 0.0)) "Padded Lenna (red bg) in RGB")

(display "testing band broadcasting")(newline)
(define img_t (image-map *
                         img1 
			(image-map (lambda (x) (if (< 100 x) 1.0 0.0))
                                   (image->green img1))))

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
                                    (list->matrix '((0.1 0.1 0.1 0.1 0.1 0.0 0.1 0.1 0.1 0.1 0.1)))
                                    2 ;;BORDER_MODE_REPEAT
                                    )
            "Box mean separable convolution")

;Testing the vigra w.r.t. Schnoerr's coherence enhancing shock filter
(define shock_img1 (shockfilter img1    2.0 6.0 0.3 1))
(define shock_img5 (shockfilter shock_img1 2.0 6.0 0.3 4))
(show-image shock_img5  "Shock-filtered image (after 5 iterations)")

;Testing the vigra w.r.t. non-local mean filter
(define nlm_img1 (nonlocalmeanfilter img1))
(show-image nlm_img1  "Non-local mean filtered image (with default args)")


#|
;Testing the vigra w.r.t. watershed segmentation and the mean image of a given image
(define (meanColorImage segmentation image)
  (let* ((region_count (inexact->exact	(+ (car (image-reduce max segmentation 0))  1)))
         (region_sizes  (make-vector region_count))
         (region_colors (make-vector region_count (make-list (image-numbands image) 0.0)))
         (new_image (copy-image image))
         (foo (image-for-each-pixel 
               (lambda (x y p)
                 (let ((region_id    (inexact->exact (image-ref segmentation x y 0))))         
                   (begin
                     (vector-set! region_sizes region_id  (+ (vector-ref region_sizes region_id) 1))
                     (vector-set! region_colors region_id  (map + p (vector-ref region_colors region_id))))))
               image))
         (normalized_colors (vector-map (lambda (color_list size)
                                               (map (lambda (color) (exact->inexact (/ color (max 1 size))))
                                                    color_list))
                                 region_colors
                                 region_sizes)))
         (image-for-each-pixel 
               (lambda (x y p)
                 (let ((region_id (inexact->exact (image-ref segmentation x y 0))))
                   (image-set! new_image x y (vector-ref normalized_colors region_id))))
                 new_image)))

(show-image (regionimagetocrackedgeimage (meanColorImage (watersheds-rg (ggradient (image->green img1) 2.0)) img1) 0.0) "img1 - watershed regions")
(show-image (regionimagetocrackedgeimage (meanColorImage (slic img1) img1) 0.0) "img1 - slic regions")
(show-image (regionimagetocrackedgeimage (meanColorImage (slic img) img) 0.0) "img - slic regions")
|#

;Testing the vigra w.r.t. watershed segmentation and the mean image of a given image
(define (meanColorImage segmentation image)
  (let ((image_stats (extractfeatures image segmentation)))
        (if (= (image-width image_stats) 34)
            (let* ((region->meanColorBand (lambda (region_id col_id)
                                                      (image-ref image_stats col_id (inexact->exact (round region_id)) 0)))
                   (region->meanColorBand_r (curryr region->meanColorBand 13))
                   (region->meanColorBand_g (curryr region->meanColorBand 14))
                   (region->meanColorBand_b (curryr region->meanColorBand 15)))
              (list (band-map region->meanColorBand_r (first segmentation))
                    (band-map region->meanColorBand_g (first segmentation))
                    (band-map region->meanColorBand_b (first segmentation))))
            (let ((band->meanColorBand (lambda (im_b st_b)
                                         (band-map (lambda (region_id)
                                                     (band-ref st_b 9 (inexact->exact (round region_id))))
                                                   im_b))))
              (map band->meanColorBand segmentation image_stats))))) 

(show-image (regionimagetocrackedgeimage (meanColorImage (watersheds-rg (ggradient (image->green img1) 2.0)) img1) 0.0) "img1 - watershed regions")
(show-image (regionimagetocrackedgeimage (meanColorImage (slic img1) img1) 0.0) "img1 - slic regions")
(show-image (regionimagetocrackedgeimage (meanColorImage (slic img) img) 0.0) "img - slic regions")

(display "saving resulting images")(newline)
(saveimage img2a   (build-path save-path "images/blox-relabeled-watersheds-uf-on-resized-gradient-image.png"))
(saveimage img2b   (build-path save-path "images/blox-relabeled-watersheds-rg-on-resized-gradient-image.png"))

(saveimage img3magnitude (build-path save-path "images/rect-fft-magnitude.png"))
(saveimage (image-map sqrt img3magnitude) (build-path save-path "images/rect-fft-sqrt-magnitude.png"))

(saveimage (second img3ifft) (build-path save-path "images/rect-fft-ifft-real.png"))
(saveimage (first  img3ifft) (build-path save-path "images/rect-fft-ifft-imag.png"))

(saveimage (second img3small_ifft) (build-path save-path "images/rect-fft-small-ifft-real.png"))
(saveimage (first  img3small_ifft) (build-path save-path "images/rect-fft-small-ifft-imag.png"))

(saveimage img4    (build-path save-path "images/blox-reflected-both.png"))
(saveimage img5    (build-path save-path "images/blox-rotated-15deg.png"))
(saveimage img5aff (build-path save-path "images/blox-aff-rotated-15deg.png"))
(saveimage img_t   (build-path save-path  "images/lenna-masked.png"))
(saveimage img7    (build-path save-path "images/blox-disttransform-on-canny.png"))
(saveimage img8    (build-path save-path "images/blox-diff_of_exp.png"))
(saveimage img9    (build-path save-path "images/blox-gsmooth-3.0.png"))
(saveimage img10   (build-path save-path "images/blox-log-3.0.png"))
(saveimage img11   (build-path save-path "images/blox-gsharpening-0.5-3.0.png"))
(saveimage img12   (build-path save-path "images/blox-sharpening-3.0.png"))
(saveimage img13   (build-path save-path "images/blox-medianfilter-3x3.png"))
(saveimage img14   (build-path save-path "images/blox-nonlineardiffusion-0.1-2.0.png"))

(saveimage (first img1_st_ec)  (build-path save-path "images/lenna-st-cornerness-rescaled.png") #t) ; rescale from min...max to 0..255!
(saveimage (first img1_st_ec)  (build-path save-path "images/lenna-st-cornerness-pure.png")     #f) ; clip to  to 0..255! If v>255 -> 255, if v<0 -> 0, else v.

(saveimage shock_img1  (build-path save-path "images/lenna-shock-s2-r6-t03-i1.png"))
(saveimage shock_img5  (build-path save-path "images/lenna-shock-s2-r6-t03-i5.png"))

(saveimage nlm_img1  (build-path save-path "images/lenna-nlm.png"))
