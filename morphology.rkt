#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;###############################################################################
;###################         Erode image                   ####################

(define vigra_discerosion_c
  (get-ffi-obj 'vigra_discerosion_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height radius) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [radius : _int]
                     -> (res :  _int))))

(define (erodeimage-band band radius)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigra_discerosion_c (band-data band) (band-data band2) width height radius)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.morphology:erodeimage: Erosion of image failed!!")))))

(define (erodeimage image radius)
  (map (lambda (band) (erodeimage-band band radius)) image))


;###############################################################################
;###################         Dilate image                   ####################

(define vigra_discdilation_c
  (get-ffi-obj 'vigra_discdilation_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height radius) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [radius : _int]
                     -> (res :  _int))))

(define (dilateimage-band band radius)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigra_discdilation_c (band-data band) (band-data band2) width height radius)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.morphology:dilateimage: Dilation of image failed!!")))))

(define (dilateimage image radius)
  (map (lambda (band) (dilateimage-band band radius)) image))

;###############################################################################
;###################         Opening  image                 ####################

(define (openingimage-band band radius)
  (dilateimage-band (erodeimage-band band radius) radius))

(define (openingimage image radius)
  (dilateimage (erodeimage image radius) radius))


;###############################################################################
;###################         Closing image                  ####################

(define (closingimage-band band radius)
  (erodeimage-band (dilateimage-band band radius) radius))

(define (closingimage image radius)
  (erodeimage (dilateimage image radius) radius))



;###############################################################################
;###################         Upwind image                   ####################

(define vigraext_upwind_c
  (get-ffi-obj 'vigraext_upwind_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 img_vector3  width height radius) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [img_vector3 : _cvector]
                     [width : _int]
                     [height : _int]
                     [radius : _float*]
                     -> (res :  _int))))

(define (upwindimage-band band signum_band radius)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2 (make-band width height 0.0))
	 (foo   (vigraext_upwind_c (band-data band) (band-data signum_band) (band-data band2) width height radius)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.morphology:upwindimage: Upwinding of image failed!!")))))

(define (upwindimage image signum_image radius)
  (map (lambda (band signum_band) (upwindimage-band band signum_band radius)) image signum_image))


(provide 
           erodeimage-band
           erodeimage
           dilateimage-band
           dilateimage
           openingimage-band
           openingimage
           closingimage-band
           closingimage
           upwindimage-band
           upwindimage
           )