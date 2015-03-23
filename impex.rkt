#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

;############ Getting Dimensions and # of Bands of an image-file    ############
(define get_width_c
    (get-ffi-obj 'get_width_c vigracket-dylib-path
                (_fun [filename : _string]
                      -> [width : _int])))
(define get_height_c
    (get-ffi-obj 'get_height_c vigracket-dylib-path
                (_fun [filename : _string]
                      -> [height : _int])))
(define get_numbands_c
    (get-ffi-obj 'get_numbands_c vigracket-dylib-path
                (_fun [filename : _string]
                      -> [height : _int])))

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
  (let* ((width (get_width_c filename))
         (height (get_height_c filename))
	 (img (make-image width height 1 0.0))
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
  (let* ((width (get_width_c filename))
         (height (get_height_c filename))
	 (img (make-image width height 3 0.0 0.0 0.0))
         (foo  (vigra_importrgbimage_c (image-data img 0)  (image-data img 1)  (image-data img 2) width height filename)))
   (case foo
      ((0) img)
      ((1) (error "Error in vigracket.impex.loadrgbimage: Image cannot be loaded by vigra!"))
      ((2) (error "Error in vigracket.impex.loadrgbimage: Image is not RGB colored"))
      ((3) (error "Error in vigracket.impex.loadrgbimage: Sizes do not match!")))))


;######    Generic (choose automatically if image is gray or colored)    #######
(define (loadimage filename)
  (case (get_numbands_c filename)
    ((1) (loadgrayimage filename))
    ((3) (loadrgbimage filename))
    (else  (error "Error in vigracket.impex.loadimage: Image has neither 1 nor 3 bands and thus cannot be loaded!"))))

(define load-image loadimage)
(define image-load loadimage)


;###############################################################################
;###################             Saving images              ####################

;###############                 Grayscale images                  #############
(define vigra_exportgrayimage_c
  (get-ffi-obj 'vigra_exportgrayimage_c vigracket-dylib-path
               (_fun (img_vector width height filename) :: [img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     -> (res :  _int))))

(define (savegrayimage image filename)
  (let* ((foo  (vigra_exportgrayimage_c (image-data image 0) (image-width image) (image-height image) filename)))
   (case foo
      ((0) #t)
      ((1) (error "Error in vigracket.impex.savegrayimage: Image cannot be saved by vigra!")))))


;###############                RGB-Color images                  #############
(define vigra_exportrgbimage_c
  (get-ffi-obj 'vigra_exportrgbimage_c vigracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b width height filename) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [filename : _string]
                     -> (res :  _int))))

(define (savergbimage image filename)
  (let* ((foo  (vigra_exportrgbimage_c (image-data image 0) (image-data image 1) (image-data image 2)
                                    (image-width image) (image-height image) filename)))
   (case foo
      ((0) #t)
      ((1) (error "Error in vigracket.impex.savergbimage: Image cannot be saved by vigra!")))))


;######    Generic (choose automatically if image is gray or colored)    #######
(define (saveimage image filename)
  (case (image-numbands image)
    ((1) (savegrayimage image filename))
    ((3) (savergbimage image filename))
    (else  (error "Error in vigracket.impex.saveimage: Image has neither 1 nor 3 bands and thus cannot be saved!"))))

(define image-save saveimage)
(define save-image saveimage)

(provide 
           loadimage
           load-image
           image-load
           saveimage
           save-image
           image-save)