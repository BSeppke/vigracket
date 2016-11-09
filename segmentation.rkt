#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;###############################################################################
;###################         Label image                    ####################

(define vigra_labelimage_c
  (get-ffi-obj 'vigra_labelimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (labelimage-band band)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigra_labelimage_c (band-data band) (band-data band2) width height)))
    (if (= foo -1)
        (error "Error in vigracket.segmentation.labelimage: Labeling of image failed!")
        band2)))
	  
(define (labelimage image)
  (map labelimage-band image))
	  
;###############################################################################
;###################      Watershed Transform (Union-Find)  ####################

(define vigra_watersheds_c
  (get-ffi-obj 'vigra_watersheds_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (watersheds-band band)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigra_watersheds_c (band-data band) (band-data band2) width height)))
    (if (= foo -1)
        (error	"Error in vigracket.segmentation.watersheds: Watershed Transform of image failed!")
        band2)))
	  
(define (watersheds image)
  (map watersheds-band image))
	  
;###############################################################################
;###################      SLIC Segmentation Algorithm        ###################    

(define vigra_slic_gray_c
  (get-ffi-obj 'vigra_slic_gray_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height seedDistance intensityScaling iterations) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [seedDistance : _int]
                     [intensityScaling : _double]
                     [iterations : _int]
                     -> (res :  _int))))

(define (slic-band band [seedDistance 15] [intensityScaling 20.0] [iterations 40])
  (let* ((width  (band-width  band))
         (height (band-height band))
         (band2 (make-band width height 0.0))
         (foo   (vigra_slic_gray_c (band-data band) (band-data band2) width height seedDistance intensityScaling iterations)))
    (if (= foo -1)
        (error	"Error in vigracket.segmentation.slic_gray: SLIC Segmentation of gray image failed!")
        band2)))

(define vigra_slic_rgb_c
  (get-ffi-obj 'vigra_slic_rgb_c vigracket-dylib-path
               (_fun (img_vector1_r img_vector1_g img_vector1_b img_vector2 width height seedDistance intensityScaling iterations) :: [img_vector1_r : _cvector]
                     [img_vector1_g : _cvector]
                     [img_vector1_b : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [seedDistance : _int]
                     [intensityScaling : _double]
                     [iterations : _int]
                     -> (res :  _int))))

(define (slic-rgb band_r band_g band_b [seedDistance 15] [intensityScaling 20.0] [iterations 40])
  (let* ((width  (band-width  band_r))
         (height (band-height band_r))
         (band2 (make-band width height 0.0))
         (foo   (vigra_slic_rgb_c (band-data band_r) (band-data band_g) (band-data band_b) (band-data band2) width height seedDistance intensityScaling iterations)))
    (if (= foo -1)
        (error	"Error in vigracket.segmentation.slic_rgb: SLIC Segmentation of rgb image failed!")
        band2)))
	  
(define (slic image [seedDistance 15] [intensityScaling 20.0] [iterations 40])
  (if (= (length image) 3)
      (list (slic-rgb (first image) (second image) (third image) seedDistance intensityScaling iterations))
      (map (curryr slic-band seedDistance intensityScaling iterations) image)))


;###############################################################################
;###################      Canny Edge-Detection              ####################

(define vigra_cannyedgeimage_c
  (get-ffi-obj 'vigra_cannyedgeimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height  scale gradient_threshold mark) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [scale : _float*]
                     [gradient_threshold  : _float*]
                     [mark : _float*]
                     -> (res :  _int))))

(define (cannyedgeimage-band band scale gradient_threshold mark)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigra_cannyedgeimage_c (band-data band) (band-data band2) width height scale gradient_threshold mark)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.segmentation:cannyedgeimage: Canny Edge Detection of image failed!")))))
	  
(define (cannyedgeimage image scale gradient_threshold mark)
  (map (lambda (band) (cannyedgeimage-band band scale gradient_threshold mark)) image))


;###############################################################################
;################    Difference of Exponential Edge-Detection  #################

(define vigra_differenceofexponentialedgeimage_c
  (get-ffi-obj 'vigra_differenceofexponentialedgeimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height  scale gradient_threshold mark) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [scale : _float*]
                     [gradient_threshold  : _float*]
                     [mark : _float*]
                     -> (res :  _int))))

(define (differenceofexponentialedgeimage-band band scale gradient_threshold mark)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigra_differenceofexponentialedgeimage_c (band-data band) (band-data band2) width height scale gradient_threshold mark)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.segmentation:differenceofexponentialedgeimage: Difference of Exponential Edge Detection of image failed!")))))

(define (differenceofexponentialedgeimage image scale gradient_threshold mark)
  (map (lambda (band) (differenceofexponentialedgeimage-band band scale gradient_threshold mark)) image))


;###############################################################################
;###################     RegionImage -> CrackEdgeImage      ####################

(define vigra_regionimagetocrackedgeimage_c
  (get-ffi-obj 'vigra_regionimagetocrackedgeimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height mark) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [mark : _float*]
                     -> (res :  _int))))

(define (regionimagetocrackedgeimage-band band mark)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band (- (* 2 width) 1) (- (* 2 height) 1) 0.0))
	 (foo   (vigra_regionimagetocrackedgeimage_c (band-data band) (band-data band2) width height mark)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.segmentation:regionimagetocrackedgeimage: Creation of CrackEdgeImage failed!")))))

(define (regionimagetocrackedgeimage image mark)
  (map (lambda (band) (regionimagetocrackedgeimage-band band mark)) image))

(provide 
           labelimage-band
           labelimage
           watersheds-band
           watersheds
           slic-band
           slic-rgb
           slic
           cannyedgeimage-band
           cannyedgeimage
           differenceofexponentialedgeimage-band
           differenceofexponentialedgeimage
           regionimagetocrackedgeimage-band
           regionimagetocrackedgeimage)