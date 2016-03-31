#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;###############################################################################
;###################          Generic Convolution           ####################

(define vigra_convolveimage_c
  (get-ffi-obj 'vigra_convolveimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 kernel_vector width height kernel_width kernel_height) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [kernel_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     [kernel_width : _int]
                     [kernel_height : _int]
                     -> (res :  _int))))

(define (convolveimage-band band kernel_matrix)
  (let* ((width  (band-width  band))
	 (height (band-height band))
         (kernel_width  (matrix-cols  kernel_matrix))
	 (kernel_height (matrix-rows  kernel_matrix))
	 (band2   (make-band width height 0.0))
         (foo     (vigra_convolveimage_c (band-data band) (band-data band2) (matrix-data kernel_matrix) width height kernel_width kernel_height)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.colvolveimage: Convolution with kernel failed!"))
      ((2) (error "Error in vigracket.filters.colvolveimage: Kernel dimensions must be odd!")))))

(define (convolveimage image kernel_matrix)
  (map (lambda (band) (convolveimage-band band kernel_matrix)) image))


;###############################################################################
;###################        Separable Convolution           ####################

(define vigra_separableconvolveimage_c
  (get-ffi-obj 'vigra_separableconvolveimage_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2 kernel_vector_h kernel_vector_v width height kernel_width kernel_height) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [kernel_vector_h : _cvector]
                     [kernel_vector_v : _cvector]
                     [width : _int]
                     [height : _int]
                     [kernel_width : _int]
                     [kernel_height : _int]
                     -> (res :  _int))))

(define (separableconvolveimage-band band kernel_matrix_h kernel_matrix_v)
  (let* ((width  (band-width  band))
	 (height (band-height band))
         (kernel_width  (matrix-cols  kernel_matrix_h))
	 (kernel_height (matrix-rows  kernel_matrix_v))
	 (band2   (make-band width height 0.0))
         (foo     (vigra_separableconvolveimage_c (band-data band) (band-data band2) 
                                                  (matrix-data kernel_matrix_h) (matrix-data kernel_matrix_v)  
                                                  width height kernel_width kernel_height)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.separableconvolveimage Convolution with kernel failed!"))
      ((2) (error "Error in vigracket.filters.separableconvolveimage Kernel dimensions must be odd!")))))
	 
(define (separableconvolveimage image kernel_matrix_h kernel_matrix_v)
  (map (lambda (band) (separableconvolveimage-band band  kernel_matrix_h kernel_matrix_v)) image))


;###############################################################################
;###################          Gaussian Smoothing            ####################

(define vigra_gaussiansmoothing_c
  (get-ffi-obj 'vigra_gaussiansmoothing_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height sigma) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [sigma : _float*]
                     -> (res :  _int))))

(define (gsmooth-band band sigma)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_gaussiansmoothing_c (band-data band) (band-data band2) width height sigma)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.gsmooth: Gaussian smoothing failed!")))))

(define (gsmooth image sigma)
  (map (lambda (band) (gsmooth-band band sigma)) image))


;###############################################################################
;###############      Gaussian Gradient (nablaX & nablaY)     ##################

(define vigra_gaussiangradient_c
  (get-ffi-obj 'vigra_gaussiangradient_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  img_vector3  width height sigma) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [img_vector3 : _cvector]
                     [width : _int]
                     [height : _int]
                     [sigma : _float*]
                     -> (res :  _int))))

(define (gaussiangradient-band band sigma)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (band3   (make-band width height 0.0))
	 (foo   (vigra_gaussiangradient_c (band-data band) (band-data band2) (band-data band3) width height sigma)))
    (case foo
      ((0) (list band2 band3))
      ((1) (error "Error in vigracket.filters.gaussiangradient: Gaussian gradient calculation (nX, nY) failed!")))))

(define (gaussiangradient image sigma)
  (pivot-list  (map (lambda (band) (gaussiangradient-band band sigma)) image)))

;###############################################################################
;###################      Gaussian Gradient (Magnitude)     ####################

(define vigra_gaussiangradientmagnitude_c
  (get-ffi-obj 'vigra_gaussiangradientmagnitude_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height sigma) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [sigma : _float*]
                     -> (res :  _int))))

(define (ggradient-band band sigma)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_gaussiangradientmagnitude_c (band-data band) (band-data band2) width height sigma)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.ggradient: Gaussian gradient magnitude failed!")))))

(define (ggradient image sigma)
  (map (lambda (band) (ggradient-band band sigma)) image))


;###############################################################################
;###################        Laplacian Of Gaussian           ####################

(define vigra_laplacianofgaussian_c
  (get-ffi-obj 'vigra_laplacianofgaussian_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height scale) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [scale : _float*]
                     -> (res :  _int))))

(define (laplacianofgaussian-band band scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_laplacianofgaussian_c (band-data band) (band-data band2) width height scale)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.laplacianofgaussian: Laplacian of Gaussian failed!")))))

(define (laplacianofgaussian image scale)
  (map (lambda (band)  (laplacianofgaussian-band band scale)) image))
  

;###############################################################################
;#############    Hessian Matrix of 2. order deriv gaussians      ##############

(define vigra_hessianmatrixofgaussian_c
  (get-ffi-obj 'vigra_hessianmatrixofgaussian_c vigracket-dylib-path
               (_fun (img_vector1 img_vector_xx img_vector_xy img_vector_yy width height scale) :: [img_vector1 : _cvector]
                     [img_vector_xx : _cvector]
                     [img_vector_xy : _cvector]
                     [img_vector_yy : _cvector]
                     [width : _int]
                     [height : _int]
                     [scale : _float*]
                     -> (res :  _int))))

(define (hessianmatrixofgaussian-band band scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band_xx   (make-band width height 0.0))
	 (band_xy   (make-band width height 0.0))
	 (band_yy   (make-band width height 0.0))
	 (foo   (vigra_hessianmatrixofgaussian_c (band-data band) (band-data band_xx) (band-data band_xy) (band-data band_yy) width height scale)))
    (case foo
      ((0) (list band_xx band_xy band_yy))
      ((1) (error "Error in vigracket.filters.hessianmatrixofgaussian: Calculation of Hessian Matrix (of gaussian 2. deriv.) failed!")))))

(define (hessianmatrixofgaussian image scale)
  (pivot-list (map (lambda (band)  (hessianmatrixofgaussian-band band scale)) image)))

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
	 (band_xx   (make-band width height 0.0))
	 (band_xy   (make-band width height 0.0))
	 (band_yy   (make-band width height 0.0))
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
	 (band_xx   (make-band width height 0.0))
	 (band_xy   (make-band width height 0.0))
	 (band_yy   (make-band width height 0.0))
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
	 (band_xx   (make-band width height 0.0))
	 (band_xy   (make-band width height 0.0))
	 (band_yy   (make-band width height 0.0))
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
	 (tensor_lEV_band   (make-band width height 0.0))
	 (tensor_sEV_band   (make-band width height 0.0))
	 (tensor_ang_band   (make-band width height 0.0))
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
	 (trace_band   (make-band width height 0.0))
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
	 (tensor_edgeness      (make-band width height 0.0))
	 (tensor_orientation   (make-band width height 0.0))
	 (tensor_cornerness    (make-band width height 0.0))
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
	 (tensor_hgxx_band   (make-band width height 0.0))
	 (tensor_hgxy_band   (make-band width height 0.0))
	 (tensor_hgyy_band   (make-band width height 0.0))
	 (foo   (vigra_hourglassfilter_c (band-data (first bands)) (band-data (second bands)) (band-data (third bands)) (band-data tensor_hgxx_band ) (band-data tensor_hgxy_band) (band-data tensor_hgyy_band) width height sigma rho)))
    (case foo
      ((0) (list tensor_hgxx_band tensor_hgxy_band tensor_hgyy_band ))
      ((1) (error "Error in vigracket.filters.hourglassfilter:  Hourglass-Filtering  of the Tensor failed!")))))

(define (hourglassfilter image sigma rho)
  (pivot-list (map (lambda (band) (hourglassfilter-band band sigma rho)) (pivot-list image))))

;###############################################################################
;###################          Gaussian Sharpening           ####################

(define vigra_gaussiansharpening_c
  (get-ffi-obj 'vigra_gaussiansharpening_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height sharpening_factor scale) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [sharpening_factor : _float*]
                     [scale : _float*]
                     -> (res :  _int))))

(define (gsharpening-band band sharpening_factor scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_gaussiansharpening_c (band-data band) (band-data band2) width height  sharpening_factor scale)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.gsharpening: Gaussian sharpening failed!")))))

(define (gsharpening image sharpening_factor scale)
  (map (lambda (band) (gsharpening-band band sharpening_factor scale)) image))
  
  
;###############################################################################
;###################            Simple Sharpening           ####################

(define vigra_simplesharpening_c
  (get-ffi-obj 'vigra_simplesharpening_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height sharpening_factor) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [sharpening_factor : _float*]
                     -> (res :  _int))))

(define (sharpening-band band sharpening_factor)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_simplesharpening_c (band-data band) (band-data band2) width height sharpening_factor)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.sharpening: Simple sharpening failed!")))))


(define (sharpening image sharpening_factor)
  (map (lambda (band) (sharpening-band band sharpening_factor)) image))

;###############################################################################
;###################          Nonlinear Diffusion           ####################

(define vigra_nonlineardiffusion_c
  (get-ffi-obj 'vigra_nonlineardiffusion_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height edge_threshold scale) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [edge_threshold : _float*]
                     [scale : _float*]
                     -> (res :  _int))))

(define (nonlineardiffusion-band band edge_threshold scale)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_nonlineardiffusion_c (band-data band) (band-data band2) width height edge_threshold scale)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracl.filters.nonlineardiffusion: Non linear Diffusion failed!!")))))

(define (nonlineardiffusion image edge_threshold scale)
  (map (lambda (band) (nonlineardiffusion-band band edge_threshold scale)) image))


;###############################################################################
;###################          Distance Transform            ####################

(define vigra_distancetransform_c
  (get-ffi-obj 'vigra_distancetransform_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height background_label norm) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [background_label : _float*]
                     [norm : _int]
                     -> (res :  _int))))

(define (distancetransform-band band background_label norm)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height 0.0))
	 (foo   (vigra_distancetransform_c (band-data band) (band-data band2) width height background_label norm)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.distancetransform: Distance transformation failed!!"))
      ((2) (error "Error in vigracket.filters.distancetransform: Norm must be in {0,1,2} !!")))))

(define (distancetransform image background_label norm)
  (map (lambda (band) (distancetransform-band band background_label norm)) image))

;###############################################################################
;###################            Shock Filter                ####################

(define vigra_shockfilter_c
  (get-ffi-obj 'vigra_shockfilter_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height sigma rho upwind_factor_h iterations) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [sigma : _float*]
                     [rho : _float*]
                     [upwind_factor_h : _float*]
                     [iterations : _int]
                     -> (res :  _int))))

(define (shockfilter-band band sigma rho upwind_factor_h [iterations 1])
  (let* ((width  (band-width  band))
         (height (band-height band))
         (band2  (make-band width height 0.0))
         (foo   (vigra_shockfilter_c (band-data band) (band-data band2) width height sigma rho upwind_factor_h iterations)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.shockfilter: Distance transformation failed!!"))
      ((2) (error "Error in vigracket.filters.shockfilter: Iterations must be > 0 !!")))))

(define (shockfilter image sigma rho upwind_factor_h [iterations 1])
  (map (lambda (band) (shockfilter-band band sigma rho upwind_factor_h iterations)) image))

(provide convolveimage-band
           convolveimage
           separableconvolveimage-band
           separableconvolveimage
           gsmooth-band
           gsmooth
           gaussiangradient-band
           gaussiangradient
           ggradient-band
           ggradient
           laplacianofgaussian-band
           laplacianofgaussian
           hessianmatrixofgaussian-band
           hessianmatrixofgaussian
           structuretensor-band
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
           hourglassfilter
           gsharpening-band
           gsharpening
           sharpening-band
           sharpening
           nonlineardiffusion-band
           nonlineardiffusion
           distancetransform-band
           distancetransform
           shockfilter-band
           shockfilter)