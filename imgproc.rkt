#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;###############################################################################
;###################         Resize image                   ####################

(define vigra_resizeimage_c
  (get-ffi-obj 'vigra_resizeimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height width2 height2 resize_mode)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [width2 : _int]
                        [height2 : _int]
                        [resize_mode : _int]
                     -> [res :  _int])))

(define (resizeimage-band  band width2 height2 [resize_mode 1])
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2  (make-band width2 height2))
	 (foo    (vigra_resizeimage_c (band-data band) (band-data band2) width height width2 height2 resize_mode)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:resizeimage: Resize of image failed!!"))
      ((2) (error "Error in vigracket.imgproc:resizeimage: Resize mode must be in {0,1,2,3,4}!!")))))

(define (resizeimage image width2 height2 [resize_mode 1])
  (map (lambda (band) (resizeimage-band band width2 height2 resize_mode)) image))
  

;###############################################################################
;###################         Rotate image                   ####################

(define vigra_rotateimage_c
  (get-ffi-obj 'vigra_rotateimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height angle resize_mode) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [angle : _float*]
                     [resize_mode : _int]
                     -> [res :  _int])))

(define (rotateimage-band band angle [resize_mode 1])
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_rotateimage_c  (band-data band) (band-data band2) width height angle resize_mode)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:rotateimage: Rotation of image failed!!"))
      ((2) (error "Error in vigracket.imgproc:rotateimage: Resize mode must be in {0,1,2,3,4}!!")))))

(define (rotateimage image angle [resize_mode 1])
  (map (lambda (band) (rotateimage-band  band angle resize_mode)) image))

;###############################################################################
;###################         Affine transform image         ####################

(define vigra_affinewarpimage_c
  (get-ffi-obj 'vigra_affinewarpimage_c vigracket-dylib-path
               (_fun (img_vector1  affineMat img_vector2  width height resize_mode)
                  :: [img_vector1 : _cvector]
                     [affineMat   : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [resize_mode : _int]
                     -> [res :  _int])))

(define (affinewarpimage-band band affinematrix [resize_mode 1])
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2  (make-band width height 0.0))
	 (foo    (vigra_affinewarpimage_c (band-data band) (matrix-data affinematrix) (band-data band2) width height resize_mode)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:affinewarpimage: Affine Warp of image failed!!"))
      ((2) (error "Error in vigracket.imgproc:affinewarpimage: Resize mode must be in {0,1,2,3,4}!!")))))

(define (affinewarpimage image affinematrix [resize_mode 1])
  (map (lambda (band) (affinewarpimage-band band affinematrix resize_mode)) image))

  
;###############################################################################
;###################         Reflect image                  ####################

(define vigra_reflectimage_c
  (get-ffi-obj 'vigra_reflectimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height reflect_mode) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [reflect_mode : _int]
                     -> [res :  _int])))

(define (reflectimage-band band reflect_mode)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2  (make-band width height))
	 (foo    (vigra_reflectimage_c (band-data band) (band-data band2) width height reflect_mode)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:reflectimage: Reflection of image failed!!"))
      ((2) (error "Error in vigracket.imgproc:reflectimage: Reflection mode must be in {1 (= horizontal), 2 (= vertical), 3 (=both)}!!")))))
	  
	  
(define (reflectimage image reflect_mode)
  (map (lambda (band) (reflectimage-band band reflect_mode)) image))

;###############################################################################
;###################         Fast Fourier Transform         ####################

(define vigra_fouriertransform_c
  (get-ffi-obj 'vigra_fouriertransform_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2   img_vector3  width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (fouriertransform-band band)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2  (make-band width height))
	 (band3  (make-band width height))
	 (foo   (vigra_fouriertransform_c (band-data band)  (band-data band2) (band-data band3) width height)))
    (case foo
      ((0) (list band2 band3))
      ((1) (error "Error in vigracket.imgproc:fouriertransform: FastFourier Transform of image failed!!")))))


(define (fouriertransform image)
  (pivot-list (map fouriertransform-band image)))

;###############################################################################
;###################     Inverse Fast Fourier Transform     ####################

(define vigra_fouriertransforminverse_c
  (get-ffi-obj 'vigra_fouriertransforminverse_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 img_vector4 width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [img_vector4 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (fouriertransforminverse-band band_real band_imag)
  (let* ((width  (band-width  band_real))
	 (height (band-height band_real))
	 (band3  (make-band width height))
	 (band4  (make-band width height))
	 (foo   (vigra_fouriertransforminverse_c (band-data band_real) (band-data band_imag) (band-data band3) (band-data band4) width height)))
    (case foo
      ((0) (list band3 band4))
      ((1) (error "Error in vigracket.imgproc:fouriertransforminverse: Inverse FastFourier Transform of image failed!!")))))


(define (fouriertransforminverse fft_image)
  (pivot-list (map fouriertransforminverse-band (first fft_image) (second fft_image))))

;###############################################################################
;###################       Fast Cross Correlation           ####################

(define vigra_fastcrosscorrelation_c
  (get-ffi-obj 'vigra_fastcrosscorrelation_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height width2 height2)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                        [width2 : _int]
                        [height2 : _int]
                     -> [res :  _int])))

(define (fastcrosscorrelation-band band mask_band)
  (let* ((width  (band-width  band))
         (height (band-height band))
         (mask_width  (band-width  mask_band))
         (mask_height (band-height mask_band))
         (band2  (make-band width height 0.0))
         (foo    (vigra_fastcrosscorrelation_c (band-data band) (band-data mask_band) (band-data band2) width height mask_width mask_height)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:fastcrosscorrelation: Fast cross-correlation of image failed!!"))
      ((2) (error "Error in vigracket.imgproc:fastcrosscorrelation: Mask width and height need to be odd!!")))))

(define (fastcrosscorrelation image mask)
  (map fastcrosscorrelation-band image mask))


;###############################################################################
;###################    Fast Normalized Cross Correlation   ####################

(define vigra_fastnormalizedcrosscorrelation_c
  (get-ffi-obj 'vigra_fastnormalizedcrosscorrelation_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height width2 height2)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                        [width2 : _int]
                        [height2 : _int]
                     -> [res :  _int])))

(define (fastnormalizedcrosscorrelation-band band mask_band)
  (let* ((width  (band-width  band))
         (height (band-height band))
         (mask_width  (band-width  mask_band))
         (mask_height (band-height mask_band))
		 (band2 (make-band width height 0.0))
         (foo    (vigra_fastnormalizedcrosscorrelation_c (band-data band) (band-data mask_band) (band-data band2) width height mask_width mask_height)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:fastnormalizedcrosscorrelation: Fast normalized cross-correlation of image failed!!"))
      ((2) (error "Error in vigracket.imgproc:fastnormalizedcrosscorrelation: Mask width and height need to be odd!!")))))

(define (fastnormalizedcrosscorrelation image mask)
  (map fastnormalizedcrosscorrelation-band image mask))

;###############################################################################
;###################           Local Maxima Extraction      ####################

(define vigra_localmaxima_c
  (get-ffi-obj 'vigra_localmaxima_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 width height eight_connectivity marker threshold allow_at_border allow_plateaus plateau_epsilon)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [eight_connectivity : _bool]
                        [marker : _float*]
                        [threshold : _float*]
                        [allow_at_border : _bool]
                        [allow_plateaus : _bool]
                        [plateau_epsilon : _float]
                     -> [res :  _int])))

(define (localmaxima-band band [eight_connectivity #t] [marker 1.0] [threshold 0.0] [allow_at_border #f] [allow_plateaus #f] [plateau_epsilon 1.0])
  (let* ((width  (band-width  band))
         (height (band-height band))
         (band2  (make-band width height 0.0))
         (foo   (vigra_localmaxima_c (band-data band) (band-data band2) width height eight_connectivity marker threshold allow_at_border allow_plateaus plateau_epsilon)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:localmaxima: Finding local maxima of image failed!!")))))


(define (localmaxima image [eight_connectivity #t])
  (map (curryr localmaxima-band eight_connectivity) image))

;###############################################################################
;###################           Local Minima Extraction      ####################

(define vigra_localminima_c
  (get-ffi-obj 'vigra_localminima_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 width height eight_connectivity marker threshold allow_at_border allow_plateaus plateau_epsilon)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [eight_connectivity : _bool]
                        [marker : _float*]
                        [threshold : _float*]
                        [allow_at_border : _bool]
                        [allow_plateaus : _bool]
                        [plateau_epsilon : _float]
                     -> [res :  _int])))

(define (localminima-band band [eight_connectivity #t] [marker 1.0] [threshold 255.0] [allow_at_border #f] [allow_plateaus #f] [plateau_epsilon 1.0])
  (let* ((width  (band-width  band))
         (height (band-height band))
	 	 (band2 (make-band width height 0.0))
         (foo   (vigra_localminima_c (band-data band) (band-data band2) width height eight_connectivity marker threshold allow_at_border allow_plateaus plateau_epsilon)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:localminima: Finding local minima of image failed!!")))))


(define (localminima image [eight_connectivity #t])
  (map (curryr localminima-band eight_connectivity) image))

;###############################################################################
;###################            Sub image                   ####################

(define vigra_subimage_c
  (get-ffi-obj 'vigra_subimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height left upper right lower)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [left : _int]
                        [upper : _int]
                        [right : _int]
                        [lower : _int]
                     -> [res :  _int])))

(define (subimage-band band left upper right lower)
  (let* ((width  (band-width  band))
         (height (band-height band))
         (cut_width (- right left))
         (cut_height (- lower upper))
         (band2  (make-band cut_width cut_height))
         (foo    (vigra_subimage_c (band-data band) (band-data band2) width height left upper right lower)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:subimage: Subimage creation failed!!"))
      ((2) (error "Error in vigracket.imgproc:subimage: Constraints not fullfilled: left < right, upper < lower, right - left <= width, lower - upper <= height")))))

(define (subimage image left upper right lower)
  (map (lambda (band) (subimage-band band left upper right lower)) image))


;###############################################################################
;###################         Padding image                  ####################

(define vigra_paddimage_c
  (get-ffi-obj 'vigra_paddimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height left upper right lower)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [left : _int]
                        [upper : _int]
                        [right : _int]
                        [lower : _int]
                     -> [res :  _int])))

(define (paddimage-band band left upper right lower [value 0.0])
  (let* ((width  (band-width  band))
         (height (band-height band))
         (padd_width (+ right width left))
         (padd_height (+ lower height upper))
         (band2  (make-band padd_width padd_height value))
         (foo    (vigra_paddimage_c (band-data band) (band-data band2) width height left upper right lower)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.imgproc:paddimage: Padding image creation failed!!"))
      ((2) (error "Error in vigracket.imgproc:paddimage: Constraints not fullfilled: left & right >= 0, upper & lower >= 0")))))

(define (paddimage image left upper right lower [value '()])
  (let* ((band_count (image-numbands image))
         (fill_value (if (empty? value)
                         (make-list band_count 0.0)
                         value)))
    (map (lambda (band band_value) (paddimage-band band left upper right lower band_value)) image fill_value)))


;###############################################################################
;###################        Clipping Images to Min..Max     ####################

(define vigra_clipimage_c
  (get-ffi-obj 'vigra_clipimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 width height low upp)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [width : _int]
                        [height : _int]
                        [low : _float*]
                        [upp : _float*]
                     -> [res :  _int])))

(define (clipimage-band band [low 0] [upp 255])
  (let* ((width  (band-width  band))
         (height (band-height band))
	 	 (band2 (make-band width height 0.0))
         (foo   (vigra_clipimage_c (band-data band) (band-data band2) width height low upp)))
    (if (= foo 0)
      band2
      (error "Error in vigracket.imgproc:clipimage: Clipping of image failed!!"))))


(define (clipimage image [low 0] [upp 255])
  (map (curryr clipimage-band low upp) image))

;###############################################################################
;###################           Image addition               ####################

(define vigra_imageplusimage_c
  (get-ffi-obj 'vigra_imageplusimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define vigra_imageplusvalue_c
  (get-ffi-obj 'vigra_imageplusvalue_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 value width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [value : _float*]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (image+-band band1 band2_or_value)
  (let* ((width  (band-width  band1))
         (height (band-height band1))
	 (band3 (make-band width height 0.0))
         (foo    (if (number? band2_or_value)
                     (vigra_imageplusvalue_c (band-data band1) (band-data band3) band2_or_value width height)
                     (vigra_imageplusimage_c (band-data band1) (band-data band2_or_value) (band-data band3) width height))))
    (case foo
      ((0) band3)
      ((1) (error "Error in vigracket.imgproc.image+: Addition of images failed!!")))))

(define (image+ image1 image2)
  (map image+-band image1 image2))


;###############################################################################
;###################           Image subtraction            ####################

(define vigra_imageminusimage_c
  (get-ffi-obj 'vigra_imageminusimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define vigra_imageminusvalue_c
  (get-ffi-obj 'vigra_imageminusvalue_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 value width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [value : _float*]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (image--band band1 band2_or_value)
  (let* ((width  (band-width  band1))
         (height (band-height band1))
	 (band3 (make-band width height 0.0))
         (foo    (if (number? band2_or_value)
                     (vigra_imageminusvalue_c (band-data band1) (band-data band3) band2_or_value width height)
                     (vigra_imageminusimage_c (band-data band1) (band-data band2_or_value) (band-data band3) width height))))
    (case foo
      ((0) band3)
      ((1) (error "Error in vigracket.imgproc.image-: Subtraction of images failed!!")))))

(define (image- image1 image2)
  (map image--band image1 image2))



;###############################################################################
;###################           Image multiplication         ####################

(define vigra_imagemultimage_c
  (get-ffi-obj 'vigra_imagemultimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define vigra_imagemultvalue_c
  (get-ffi-obj 'vigra_imagemultvalue_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 value width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [value : _float*]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (image*-band band1 band2_or_value)
  (let* ((width  (band-width  band1))
         (height (band-height band1))
	 (band3 (make-band width height 0.0))
         (foo    (if (number? band2_or_value)
                     (vigra_imagemultvalue_c (band-data band1) (band-data band3) band2_or_value width height)
                     (vigra_imagemultimage_c (band-data band1) (band-data band2_or_value) (band-data band3) width height))))
    (case foo
      ((0) band3)
      ((1) (error "Error in vigracket.imgproc.image*: Multiplication of images failed!!")))))

(define (image* image1 image2)
  (map image*-band image1 image2))




;###############################################################################
;###################              Image dividing            ####################

(define vigra_imagedivideimage_c
  (get-ffi-obj 'vigra_imagedivideimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define vigra_imagedividevalue_c
  (get-ffi-obj 'vigra_imagedividevalue_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 value width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [value : _float*]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (image/-band band1 band2_or_value)
  (let* ((width  (band-width  band1))
         (height (band-height band1))
	 (band3 (make-band width height 0.0))
         (foo    (if (number? band2_or_value)
                     (vigra_imagedividevalue_c (band-data band1) (band-data band3) band2_or_value width height)
                     (vigra_imagedivideimage_c (band-data band1) (band-data band2_or_value) (band-data band3) width height))))
    (case foo
      ((0) band3)
      ((1) (error "Error in vigracket.imgproc.image/: Dividing of images failed!!")))))

(define (image/ image1 image2)
  (map image/-band image1 image2))



;###############################################################################
;###################          Image raised to power         ####################

(define vigra_imagepowimage_c
  (get-ffi-obj 'vigra_imagepowimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3 width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [img_vector3 : _cvector]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define vigra_imagepowvalue_c
  (get-ffi-obj 'vigra_imagepowvalue_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 value width height)
                     :: [img_vector1 : _cvector]
                        [img_vector2 : _cvector]
                        [value : _float*]
                        [width : _int]
                        [height : _int]
                     -> [res :  _int])))

(define (image^-band band1 band2_or_value)
  (let* ((width  (band-width  band1))
         (height (band-height band1))
	 (band3 (make-band width height 0.0))
         (foo    (if (number? band2_or_value)
                     (vigra_imagepowvalue_c (band-data band1) (band-data band3) band2_or_value width height)
                     (vigra_imagepowimage_c (band-data band1) (band-data band2_or_value) (band-data band3) width height))))
    (case foo
      ((0) band3)
      ((1) (error "Error in vigracket.imgproc.image^: Dividing of images failed!!")))))

(define (image^ image1 image2)
  (map image^-band image1 image2))



(provide
           resizeimage-band
           resizeimage
           rotateimage-band
           rotateimage
           affinewarpimage-band
           affinewarpimage
           reflectimage-band
           reflectimage
           fouriertransform-band
           fouriertransform
           fouriertransforminverse-band
           fouriertransforminverse
           fastcrosscorrelation-band
           fastcrosscorrelation
           fastnormalizedcrosscorrelation-band
           fastnormalizedcrosscorrelation
           localmaxima-band
           localmaxima
           localminima-band
           localminima
           subimage-band
           subimage
           paddimage-band
           paddimage
           clipimage-band
           clipimage
           image+-band
           image+
           image--band
           image-
           image*-band
           image*
           image/-band
           image/
           image^-band
           image^)