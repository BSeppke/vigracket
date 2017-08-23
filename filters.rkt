#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)
  
;###############################################################################
;###################          Generic Convolution           ####################

(define vigra_convolveimage_c
  (get-ffi-obj 'vigra_convolveimage_c vigracket-dylib-path
               (_fun (img_vector1 kernel_vector img_vector2 width height kernel_width kernel_height border_treatment) :: [img_vector1 : _cvector]
                     [kernel_vector : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [kernel_width : _int]
                     [kernel_height : _int]
                     [border_treatment : _int]
                     -> (res :  _int))))

(define (convolveimage-band band kernel_matrix [border_treatment 3])
  (let* ((width  (band-width  band))
	 (height (band-height band))
         (kernel_width  (matrix-cols  kernel_matrix))
	 (kernel_height (matrix-rows  kernel_matrix))
	 (band2   (make-band width height))
         (foo     (vigra_convolveimage_c (band-data band) (matrix-data kernel_matrix)
                                         (band-data band2)
                                         width height kernel_width kernel_height
                                         border_treatment)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.colvolveimage: Convolution with kernel failed!"))
      ((2) (error "Error in vigracket.filters.colvolveimage: Kernel dimensions must be odd!"))
      ((3) (error "Error in vigracket.filters.colvolveimage: Border treatment mode must be in [0, ..., 5]!")))))

(define (convolveimage image kernel_matrix [border_treatment 3])
  (map (lambda (band) (convolveimage-band band kernel_matrix border_treatment)) image))


;###############################################################################
;###################        Separable Convolution           ####################

(define vigra_separableconvolveimage_c
  (get-ffi-obj 'vigra_separableconvolveimage_c vigracket-dylib-path
               (_fun (img_vector1 kernel_vector_h kernel_vector_v img_vector2 width height kernel_width kernel_height border_treatment) :: [img_vector1 : _cvector]
                     [kernel_vector_h : _cvector]
                     [kernel_vector_v : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [kernel_width : _int]
                     [kernel_height : _int]
                     [border_treatment : _int]
                     -> (res :  _int))))

(define (separableconvolveimage-band band kernel_matrix_h kernel_matrix_v [border_treatment 3])
  (let* ((width  (band-width  band))
	 (height (band-height band))
         (kernel_width  (matrix-cols  kernel_matrix_h))
	 (kernel_height (matrix-rows  kernel_matrix_v))
	 (band2   (make-band width height))
         (foo     (vigra_separableconvolveimage_c (band-data band) (matrix-data kernel_matrix_h) (matrix-data kernel_matrix_v)
                                                  (band-data band2) 
                                                  width height kernel_width kernel_height
                                                  border_treatment)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.separableconvolveimage Convolution with kernel failed!"))
      ((2) (error "Error in vigracket.filters.separableconvolveimage Kernel dimensions must be odd!"))
      ((3) (error "Error in vigracket.filters.separableconvolveimage Border treatment mode must be in [0, ..., 5]!")))))
	 
(define (separableconvolveimage image kernel_matrix_h kernel_matrix_v  [border_treatment 3])
  (map (lambda (band) (separableconvolveimage-band band  kernel_matrix_h kernel_matrix_v border_treatment)) image))


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
	 (band2   (make-band width height))
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
	 (band2   (make-band width height))
	 (band3   (make-band width height))
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
	 (band2   (make-band width height))
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
	 (band2   (make-band width height))
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
	 (band_xx   (make-band width height))
	 (band_xy   (make-band width height))
	 (band_yy   (make-band width height))
	 (foo   (vigra_hessianmatrixofgaussian_c (band-data band) (band-data band_xx) (band-data band_xy) (band-data band_yy) width height scale)))
    (case foo
      ((0) (list band_xx band_xy band_yy))
      ((1) (error "Error in vigracket.filters.hessianmatrixofgaussian: Calculation of Hessian Matrix (of gaussian 2. deriv.) failed!")))))

(define (hessianmatrixofgaussian image scale)
  (pivot-list (map (lambda (band)  (hessianmatrixofgaussian-band band scale)) image)))

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
	 (band2   (make-band width height))
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
	 (band2   (make-band width height))
	 (foo   (vigra_simplesharpening_c (band-data band) (band-data band2) width height sharpening_factor)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.sharpening: Simple sharpening failed!")))))


(define (sharpening image sharpening_factor)
  (map (lambda (band) (sharpening-band band sharpening_factor)) image))

;###############################################################################
;###################          Median Filtering              ####################

(define vigra_medianfilter_c
  (get-ffi-obj 'vigra_medianfilter_c vigracket-dylib-path
               (_fun (img_vector1 img_vector2  width height window_width window_height) :: [img_vector1 : _cvector]
                     [img_vector2 : _cvector]
                     [width : _int]
                     [height : _int]
                     [window_width : _int]
                     [window_height : _int]
                     -> (res :  _int))))

(define (medianfilter-band band window_width window_height)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (band2   (make-band width height))
	 (foo   (vigra_medianfilter_c (band-data band) (band-data band2) width height window_width window_height)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.medianfilter: Median filtering failed!!")))))

(define (medianfilter image window_width window_height)
  (map (lambda (band) (medianfilter-band band window_width window_height)) image))

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
	 (band2   (make-band width height))
	 (foo   (vigra_nonlineardiffusion_c (band-data band) (band-data band2) width height edge_threshold scale)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.nonlineardiffusion: Non linear Diffusion failed!!")))))

(define (nonlineardiffusion image edge_threshold scale)
  (map (lambda (band) (nonlineardiffusion-band band edge_threshold scale)) image))

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
         (band2  (make-band width height))
         (foo   (vigra_shockfilter_c (band-data band) (band-data band2) width height sigma rho upwind_factor_h iterations)))
    (case foo
      ((0) band2)
      ((1) (error "Error in vigracket.filters.shockfilter: Distance transformation failed!!"))
      ((2) (error "Error in vigracket.filters.shockfilter: Iterations must be > 0 !!")))))

(define (shockfilter image sigma rho upwind_factor_h [iterations 1])
  (map (lambda (band) (shockfilter-band band sigma rho upwind_factor_h iterations)) image))

(provide   convolveimage-band
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
           gsharpening-band
           gsharpening
           sharpening-band
           sharpening
           medianfilter-band
           medianfilter
           nonlineardiffusion-band
           nonlineardiffusion
           shockfilter-band
           shockfilter)