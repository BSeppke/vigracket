#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;###############################################################################
;#########  Structure Tensor using inner and outer gaussians      ##############

(define vigra_structuretensor_c
  (get-ffi-obj 'vigra_structuretensor_c vigracket-dylib-path
               (_fun (img_vector1 img_vector_xx img_vector_xy img_vector_yy width height inner_scale outer_scale) :: [img_vector1 : _cvector]
                     [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [width : _int]
                     [height : _int]
                     [inner_scale : _float*]
                     [outer_scale : _float*]
                     -> (res :  _int))))

(define (structuretensor-band band inner_scale outer_scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band_xx   (make-band width height))
	 (band_xy   (make-band width height))
	 (band_yy   (make-band width height))
	 (foo   (vigra_structuretensor_c (band-data band) (band-data band_xx) (band-data band_xy) (band-data band_yy) width height inner_scale outer_scale)))
    (case foo
      ((0) (list band_xx band_xy band_yy))
      ((1) (error "Error in vigracket.filters.structuretensor: Calculation of Gaussian Structure Tensor failed!")))))

(define (structuretensor image inner_scale outer_scale)
  (pivot-list (map (lambda (band)  (structuretensor-band band inner_scale outer_scale)) image)))

;###############################################################################
;#############               Koethes Boundary tensor              ##############

(define vigra_boundarytensor_c
  (get-ffi-obj 'vigra_boundarytensor_c vigracket-dylib-path
               (_fun (img_vector1 img_vector_xx img_vector_xy img_vector_yy width height scale) :: [img_vector1 : _cvector]
                     [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [width : _int]
                     [height : _int]
                     [scale : _float*]
                     -> (res :  _int))))

(define (boundarytensor-band band scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band_xx   (make-band width height))
	 (band_xy   (make-band width height))
	 (band_yy   (make-band width height))
	 (foo   (vigra_boundarytensor_c (band-data band) (band-data band_xx) (band-data band_xy) (band-data band_yy) width height scale)))
    (case foo
      ((0) (list band_xx band_xy band_yy))
      ((1) (error "Error in vigracket.filters.boundarytensor: Calculation of Koethes Boundary tensor failed!")))))

(define (boundarytensor image scale)
  (pivot-list (map (lambda (band)  (boundarytensor-band band scale)) image)))

;###############################################################################
;#######     Koethes Boundary tensor (without 0th order response)      #########

(define vigra_boundarytensor1_c
  (get-ffi-obj 'vigra_boundarytensor1_c vigracket-dylib-path
               (_fun (img_vector1 img_vector_xx img_vector_xy img_vector_yy width height scale) :: [img_vector1 : _cvector]
                     [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [width : _int]
                     [height : _int]
                     [scale : _float*]
                     -> (res :  _int))))

(define (boundarytensor1-band band scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band_xx   (make-band width height))
	 (band_xy   (make-band width height))
	 (band_yy   (make-band width height))
	 (foo   (vigra_boundarytensor1_c (band-data band) (band-data band_xx) (band-data band_xy) (band-data band_yy) width height scale)))
    (case foo
      ((0) (list band_xx band_xy band_yy))
      ((1) (error "Error in vigracket.filters.boundarytensor: Calculation of Koethes Boundary tensor failed!")))))

(define (boundarytensor1 image scale)
  (pivot-list (map (lambda (band)  (boundarytensor1-band band scale)) image)))

;###############################################################################
;#########     Eigenvalue Representation of a Tensor              ##############
(define vigra_tensoreigenrepresentation_c
  (get-ffi-obj 'vigra_tensoreigenrepresentation_c vigracket-dylib-path
               (_fun (img_vector_xx img_vector_xy img_vector_yy img_vector_lEV img_vector_sEV img_vector_ang width height) :: [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [img_vector_lEV : _cvector]
                     [img_vector_sEV : _cvector]
                     [img_vector_ang : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (tensoreigenrepresentation-band bands)
  (let* ((width  (band-width  (first bands)))
	 (height (band-height (first bands)))
	 (tensor_lEV_band   (make-band width height))
	 (tensor_sEV_band   (make-band width height))
	 (tensor_ang_band   (make-band width height))
	 (foo   (vigra_tensoreigenrepresentation_c (band-data (first bands)) (band-data (second bands)) (band-data (third bands)) (band-data tensor_lEV_band) (band-data tensor_sEV_band) (band-data tensor_ang_band) width height)))
    (case foo
      ((0) (list tensor_lEV_band tensor_sEV_band tensor_ang_band ))
      ((1) (error "Error in vigracket.filters.tensoreigenrepresentation: Calculation of the  Eigenvalue Representation of the Tensor failed!")))))

(define (tensoreigenrepresentation image)
  (pivot-list (map tensoreigenrepresentation-band (pivot-list image))))

;###############################################################################
;#########                    Trace of a Tensor                   ##############
(define vigra_tensortrace_c
  (get-ffi-obj 'vigra_tensortrace_c vigracket-dylib-path
               (_fun (img_vector_xx img_vector_xy img_vector_yy img_vector_trace width height) :: [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [img_vector_trace : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (tensortrace-band bands)
  (let* ((width  (band-width  (first bands)))
	 (height (band-height (first bands)))
	 (trace_band   (make-band width height))
	 (foo   (vigra_tensortrace_c (band-data (first bands)) (band-data (second bands)) (band-data (third bands)) (band-data trace_band) width height)))
    (case foo
      ((0) trace_band)
      ((1) (error "Error in vigracket.filters.tensortrace: Calculation of the trace of the Tensor failed!")))))

(define (tensortrace image)
  (map tensortrace-band (pivot-list image)))

;###############################################################################
;#########     Edge/Corner Representation of a Tensor             ##############
(define vigra_tensortoedgecorner_c
  (get-ffi-obj 'vigra_tensortoedgecorner_c vigracket-dylib-path
               (_fun (img_vector_xx img_vector_xy img_vector_yy img_vector_edgeness img_vector_orientation img_vector_cornerness width height) :: [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [img_vector_edgeness : _cvector]
                     [img_vector_orientation : _cvector]
                     [img_vector_cornerness : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (tensortoedgecorner-band bands)
  (let* ((width  (band-width  (first bands)))
	 (height (band-height (first bands)))
	 (tensor_edgeness      (make-band width height))
	 (tensor_orientation   (make-band width height))
	 (tensor_cornerness    (make-band width height))
	 (foo   (vigra_tensortoedgecorner_c (band-data (first bands)) (band-data (second bands)) (band-data (third bands)) (band-data tensor_edgeness) (band-data tensor_orientation) (band-data tensor_cornerness) width height)))
    (case foo
      ((0) (list tensor_edgeness tensor_orientation tensor_cornerness ))
      ((1) (error "Error in vigracket.filters.tensortoedgecorner: Calculation of the Edge/Corner Representation of the Tensor failed!")))))

(define (tensortoedgecorner image)
  (pivot-list (map tensortoedgecorner-band (pivot-list image))))

;###############################################################################
;#########           Hourglass-Filtering  of a Tensor             ##############
(define vigra_hourglassfilter_c
  (get-ffi-obj 'vigra_hourglassfilter_c vigracket-dylib-path
               (_fun (img_vector_xx img_vector_xy img_vector_yy img_vector_hgxx img_vector_hgxy img_vector_hgyy width height sigma rho) :: [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [img_vector_hgxx : _cvector]
                     [img_vector_hgxy : _cvector]
                     [img_vector_hgyy : _cvector]
                     [width : _int]
                     [height : _int]
                     [sigma : _float*]
                     [rho : _float*]
                     -> (res :  _int))))

(define (hourglassfilter-band bands sigma rho)
  (let* ((width  (band-width  (first bands)))
	 (height (band-height (first bands)))
	 (tensor_hgxx_band   (make-band width height))
	 (tensor_hgxy_band   (make-band width height))
	 (tensor_hgyy_band   (make-band width height))
	 (foo   (vigra_hourglassfilter_c (band-data (first bands)) (band-data (second bands)) (band-data (third bands)) (band-data tensor_hgxx_band ) (band-data tensor_hgxy_band) (band-data tensor_hgyy_band) width height sigma rho)))
    (case foo
      ((0) (list tensor_hgxx_band tensor_hgxy_band tensor_hgyy_band ))
      ((1) (error "Error in vigracket.filters.hourglassfilter:  Hourglass-Filtering  of the Tensor failed!")))))

(define (hourglassfilter image sigma rho)
  (pivot-list (map (lambda (band) (hourglassfilter-band band sigma rho)) (pivot-list image))))

(provide   structuretensor-band
           structuretensor
           boundarytensor-band
           boundarytensor
           boundarytensor1-band
           boundarytensor1
           tensoreigenrepresentation-band
           tensoreigenrepresentation
           tensortrace-band
           tensortrace
           tensortoedgecorner-band
           tensortoedgecorner
           hourglassfilter-band
           hourglassfilter)