#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require vigracket/imgproc)
(require scheme/foreign)
(unsafe!)

;###############################################################################
;###################         Label image                    ####################

(define vigra_labelimage_c
  (get-ffi-obj 'vigra_labelimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height eight_connectivity) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [eight_connectivity : _bool]
                     -> (res :  _int))))

(define vigra_labelimagewithbackground_c
  (get-ffi-obj 'vigra_labelimagewithbackground_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height eight_connectivity background)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [eight_connectivity : _bool]
                        [background  : _float*]
                     -> (res :  _int))))

(define (labelimage-band band [eight_connectivity #t] [background '()])
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height))
	 (foo   (if (number? background)
                    (vigra_labelimagewithbackground_c (band-data band) (band-data band2) width height eight_connectivity background)
                    (vigra_labelimage_c (band-data band) (band-data band2) width height eight_connectivity))))
    (if (= foo -1)
        (error "Error in vigracket.segmentation.labelimage: Labeling of image failed!")
        band2)))
	  
(define (labelimage image [eight_connectivity #t] [background '()])
  (map (curryr labelimage-band eight_connectivity background) image))
	  
;###############################################################################
;###################      Watershed Transform (Union-Find)  ####################

(define vigra_watershedsunionfind_c
  (get-ffi-obj 'vigra_watershedsunionfind_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height eight_connectivity) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [eight_connectivity : _bool]
                     -> (res :  _int))))

(define (watersheds-uf-band band [eight_connectivity #t])
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height))
	 (foo   (vigra_watershedsunionfind_c (band-data band) (band-data band2) width height eight_connectivity)))
    (if (= foo -1)
        (error	"Error in vigracket.segmentation.watershed-uf: Watershed Transform (union-find method) of image failed!")
        band2)))
	  
(define (watersheds-uf image [eight_connectivity #t])
  (map (curryr watersheds-uf-band eight_connectivity) image))
	  
;###############################################################################
;###################  Watershed Transform (Region-Growing)  ####################

(define vigra_watershedsregiongrowing_c
  (get-ffi-obj 'vigra_watershedsregiongrowing_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 width height eight_connectivity keep_contours use_turbo stop_cost)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [eight_connectivity : _bool]
                        [keep_contours : _bool]
                        [use_turbo : _bool]
                        [stop_cost : _double]
                     -> (res :  _int))))

(define (watersheds-rg-band band [seeds-band '()] [eight_connectivity #t] [keep_contours #f] [use_turbo #f] [stop-cost -1.0])
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (if (empty? seeds-band)
                    (band-map (curryr - 1.0) (labelimage-band (localminima-band band)))
                    seeds-band))
	 (foo   (vigra_watershedsregiongrowing_c (band-data band) (band-data band2) width height eight_connectivity keep_contours use_turbo stop-cost)))
    (if (= foo -1)
        (error	"Error in vigracket.segmentation.watersheds-rg: Watershed Transform (region-growing method) of image failed!")
        band2)))
	  
(define (watersheds-rg image [seeds '()][eight_connectivity #t] [keep_contours #f] [use_turbo #f] [stop-cost -1.0])
  (map (curryr watersheds-rg-band eight_connectivity keep_contours use_turbo stop-cost)
       image
       (if (empty? seeds)
           (image-map (curryr - 1.0) (labelimage (localminima image)))
           seeds)))
	  
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
         (band2 (make-band width height))
         (foo   (vigra_slic_gray_c (band-data band) (band-data band2) width height seedDistance intensityScaling iterations)))
    (if (= foo -1)
        (error	"Error in vigracket.segmentation.slic-band: SLIC Segmentation of gray image failed!")
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
         (band2 (make-band width height))
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
	 (band2 (make-band width height))
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
	 (band2 (make-band (- (* 2 width) 1) (- (* 2 height) 1)))
	 (foo   (vigra_regionimagetocrackedgeimage_c (band-data band) (band-data band2) width height mark)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.segmentation:regionimagetocrackedgeimage: Creation of CrackEdgeImage failed!")))))

(define (regionimagetocrackedgeimage image mark)
  (map (lambda (band) (regionimagetocrackedgeimage-band band mark)) image))


	  
;###############################################################################
;###################           Feature Extraction            ###################    

(define vigra_extractfeatures_gray_c
  (get-ffi-obj 'vigra_extractfeatures_gray_c  vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height max_label)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                        [max_label : _int*]
                     -> (res :  _int))))

(define (extractfeatures-band band label-band [max_label (band-reduce max label-band 0)])
  (let* ((width  (band-width  band))
         (height (band-height band))
         (band3 (make-band 22 (+ (inexact->exact (round max_label)) 1)))
         (foo   (vigra_extractfeatures_gray_c (band-data band) (band-data label-band) (band-data band3) width height max_label)))
    (if (= foo 1)
        (error	"Error in vigracket.segmentation.vigra_extractfeatures_gray_c: Region-wise feature extraction of gray image failed!")
        band3)))


(define vigra_extractfeatures_rgb_c
  (get-ffi-obj 'vigra_extractfeatures_rgb_c vigracket-dylib-path
               (_fun (img_r_vector1 img_g_vector1 img_b_vector1 img_vector2 img_vector3 width height max_label)
                     :: [img_r_vector1 : _cvector]
                        [img_g_vector1 : _cvector]
                        [img_b_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                        [max_label : _int*]
                     -> (res :  _int))))

(define (extractfeatures-rgb band_r band_g band_b label-band [max_label (band-reduce max label-band 0)])
  (let* ((width  (band-width  band_r))
         (height (band-height band_r))
         (band3 (make-band 34 (+ (inexact->exact (round max_label)) 1)))
         (foo   (vigra_extractfeatures_rgb_c (band-data band_r) (band-data band_g) (band-data band_b) (band-data label-band) (band-data band3) width height max_label)))
    (if (= foo 1)
        (error	"Error in vigracket.segmentation.vigra_extractfeatures_rgb_c: Region-wise feature extraction of gray image failed!")
        band3)))
	  
(define (extractfeatures image labels [max_label (image-reduce max labels 0)])
  (if (and (= (length image) 3) (= (length labels) 1))
      (list (extractfeatures-rgb (first image) (second image) (third image) (first labels)))
      (map extractfeatures-band image labels max_label)))

(provide 
           labelimage-band
           labelimage
           watersheds-uf-band
           watersheds-uf
           watersheds-rg-band
           watersheds-rg
           slic-band
           slic-rgb
           slic
           cannyedgeimage-band
           cannyedgeimage
           differenceofexponentialedgeimage-band
           differenceofexponentialedgeimage
           regionimagetocrackedgeimage-band
           regionimagetocrackedgeimage
           extractfeatures-band
           extractfeatures-rgb
           extractfeatures)