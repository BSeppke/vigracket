#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;############ Getting Dimensions and # of Bands of an image-file    ############
(define vigra_imagewidth_c
    (get-ffi-obj 'vigra_imagewidth_c vigracket-dylib-path
                (_fun [filename : _string]
                      -> [width : _int])))
(define vigra_imageheight_c
    (get-ffi-obj 'vigra_imageheight_c vigracket-dylib-path
                (_fun [filename : _string]
                      -> [height : _int])))
(define vigra_imagenumbands_c
    (get-ffi-obj 'vigra_imagenumbands_c vigracket-dylib-path
                (_fun [filename : _string]
                      -> [numbands : _int])))

;###############################################################################
;###################             Loading images             ####################

;###############                 Grayscale images                  #############
(define vigra_importgrayimage_c
  (get-ffi-obj 'vigra_importgrayimage_c vigracket-dylib-path
               (_fun (img_vector width height filename) :: [img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     -> (res :  _int))))


(define (loadgrayimage filename)
  (let* ((width (vigra_imagewidth_c filename))
         (height (vigra_imageheight_c filename))
	 (img (make-image width height 1))
         (foo  (vigra_importgrayimage_c (image-data img 0) width height filename)))
   (case foo
      ((0) img)
      ((1) (error "Error in vigracket.impex.loadgrayimage: Image cannot be loaded by vigra!"))
      ((2) (error "Error in vigracket.impex.loadgrayimage: Image is not grayscale!"))
      ((3) (error "Error in vigracket.impex.loadgrayimage: Sizes do not match!")))))

;###############                RGB-Color images                  #############
(define vigra_importrgbimage_c
  (get-ffi-obj 'vigra_importrgbimage_c vigracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b width height filename) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     -> (res :  _int))))


(define (loadrgbimage filename)
  (let* ((width (vigra_imagewidth_c filename))
         (height (vigra_imageheight_c filename))
	 (img (make-image width height 3))
         (foo  (vigra_importrgbimage_c (image-data img 0)  (image-data img 1)  (image-data img 2) width height filename)))
   (case foo
      ((0) img)
      ((1) (error "Error in vigracket.impex.loadrgbimage: Image cannot be loaded by vigra!"))
      ((2) (error "Error in vigracket.impex.loadrgbimage: Image is not RGB colored"))
      ((3) (error "Error in vigracket.impex.loadrgbimage: Sizes do not match!")))))


;###############                RGBA-Color images                  #############
(define vigra_importrgbaimage_c
  (get-ffi-obj 'vigra_importrgbaimage_c vigracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b img_vector_a width height filename) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [img_vector_a : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     -> (res :  _int))))


(define (loadrgbaimage filename)
  (let* ((width (vigra_imagewidth_c filename))
         (height (vigra_imageheight_c filename))
	 (img (make-image width height 4))
         (foo  (vigra_importrgbaimage_c (image-data img 0)  (image-data img 1)  (image-data img 2) (image-data img 3) width height filename)))
   (case foo
      ((0) img)
      ((1) (error "Error in vigracket.impex.loadrgbaimage: Image cannot be loaded by vigra!"))
      ((2) (error "Error in vigracket.impex.loadrgbaimage: Image is not RGBA colored"))
      ((3) (error "Error in vigracket.impex.loadrgbaimage: Sizes do not match!")))))


;######    Generic (choose automatically if image is gray or colored)    #######
(define (loadimage filename)
  (let ((band_count (vigra_imagenumbands_c filename)))
    (case band_count
      ((1) (loadgrayimage filename))
      ((3) (loadrgbimage filename))
      ((4) (loadrgbaimage filename))
      (else  (error (format "Error in vigracket.impex.loadimage: Image has neither 1 nor 3 nor 4 bands (but ~a) and thus cannot be loaded!" band_count))))))

(define load-image loadimage)
(define image-load loadimage)


;###############################################################################
;###################             Saving images              ####################

;###############                 Grayscale images                  #############
(define vigra_exportgrayimage_c
  (get-ffi-obj 'vigra_exportgrayimage_c vigracket-dylib-path
               (_fun (img_vector width height filename rescale_range) :: [img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     [rescale_range : _bool]
                     -> (res :  _int))))

(define (savegrayimage image filename [rescale_range #t])
  (let* ((foo  (vigra_exportgrayimage_c (image-data image 0) (image-width image) (image-height image) filename rescale_range)))
   (case foo
      ((0) #t)
      ((1) (error "Error in vigracket.impex.savegrayimage: Image cannot be saved by vigra!")))))


;###############                RGB-Color images                  #############
(define vigra_exportrgbimage_c
  (get-ffi-obj 'vigra_exportrgbimage_c vigracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b width height filename rescale_range) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     [rescale_range : _bool]
                     -> (res :  _int))))

(define (savergbimage image filename [rescale_range #t])
  (let* ((foo  (vigra_exportrgbimage_c (image-data image 0) (image-data image 1) (image-data image 2)
                                    (image-width image) (image-height image) filename rescale_range)))
   (case foo
      ((0) #t)
      ((1) (error "Error in vigracket.impex.savergbimage: Image cannot be saved by vigra!")))))

;###############            RGBA-Color/Alpha images               #############
(define vigra_exportrgbaimage_c
  (get-ffi-obj 'vigra_exportrgbaimage_c vigracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b img_vector_a width height filename rescale_range) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [img_vector_a : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     [rescale_range : _bool]
                     -> (res :  _int))))

(define (savergbaimage image filename [rescale_range #t])
  (let* ((foo  (vigra_exportrgbaimage_c (image-data image 0) (image-data image 1) (image-data image 2) (image-data image 3)
                                    (image-width image) (image-height image) filename rescale_range)))
   (case foo
      ((0) #t)
      ((1) (error "Error in vigracket.impex.savergbaimage: Image cannot be saved by vigra!")))))


;######    Generic (choose automatically if image is gray or colored)    #######
(define (saveimage image filename [rescale_range #t])
  (case (image-numbands image)
    ((1) (savegrayimage image filename rescale_range))
    ((3) (savergbimage image filename rescale_range))
    ((4) (savergbaimage image filename rescale_range))
    (else  (error "Error in vigracket.impex.saveimage: Image has neither 1 nor 3 nor 4 bands and thus cannot be saved!"))))

(define image-save saveimage)
(define save-image saveimage)

(provide 
           loadimage
           load-image
           image-load
           saveimage
           save-image
           image-save)