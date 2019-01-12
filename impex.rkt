#lang racket

(require vigracket/config)
(require vigracket/carray)
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








(define vigra_has_hdf5_c
    (get-ffi-obj 'vigra_has_hdf5_c vigracket-dylib-path
                (_fun -> [hasHdf5Support : _int])))

(define vigra_hdf5_numdimensions_c
    (get-ffi-obj 'vigra_hdf5_numdimensions_c vigracket-dylib-path
                (_fun    [filename : _string]
                         [pathInFile : _string]
                      -> [numDimensions : _int])))

(define vigra_hdf5_shape_c
    (get-ffi-obj 'vigra_hdf5_shape_c vigracket-dylib-path
                (_fun    [filename : _string]
                         [pathInFile : _string]
                         [shape_arr : _cvector]
                         [numDimensions : _int]
                      -> [error : _int])))

(define vigra_hdf5_importarray_c
    (get-ffi-obj 'vigra_hdf5_importarray_c vigracket-dylib-path
                (_fun    [filename : _string]
                         [pathInFile : _string]
                         [flat_arr : _cvector]
                         [shape_arr : _cvector]
                         [numDimensions : _int]
                      -> [error : _int])))

(define vigra_hdf5_exportarray_c
    (get-ffi-obj 'vigra_hdf5_exportarray_c vigracket-dylib-path
                (_fun    [flat_arr : _cvector]
                         [shape_arr : _cvector]
                         [numDimensions : _int]
                         [filename : _string]
                         [pathInFile : _string]
                      -> [error : _int])))




(define (hdf5shape filename pathInFile)
  (if (= (vigra_has_hdf5_c) 0)
      (error "HDF5 is not included in your installation of vigra_c")
      (let [(numDimensions (vigra_hdf5_numdimensions_c filename pathInFile))]
        (if (> numDimensions 0)
            (let* [(shape_arr (make-cvector _int numDimensions))
                   (result    (vigra_hdf5_shape_c filename pathInFile shape_arr numDimensions))]
              (if (= result 0)
                  shape_arr
                  (error "Reading of shape for HDF5 file failed.")))
            (error "Reading of number of dimensions for HDF5 file failed.")))))
              

(define (loadarray filename pathInFile)
  (let* [(shape_arr (hdf5shape filename pathInFile))
         (shape_list (map inexact->exact (cvector->list shape_arr)))
         (totalLen  (apply * shape_list))
         (numDimensions  (length shape_list))
         (hdf5_arr  (make-carray _float shape_list))
         (result    (vigra_hdf5_importarray_c  filename pathInFile (carray-data hdf5_arr) shape_arr numDimensions))]
    (if (= result 0)
        hdf5_arr
        (error "Loading of hdf5 array failed."))))        

(define array-load loadarray)
(define load-array loadarray)

(define (savearray arr filename pathInFile)
  (let* [(shape_list (carray-dimensions arr))
         (shape_arr (list->cvector shape_list _int))
         (numDimensions  (length shape_list))
         (result    (vigra_hdf5_exportarray_c (carray-data arr) shape_arr numDimensions filename pathInFile))]
    (if (= result 0)
        #t
        (error "Saving of hdf5 array failed."))))

(define array-save savearray)
(define save-array savearray)

(provide
           loadimage
           load-image
           image-load
           
           saveimage
           save-image
           image-save
           
           loadarray
           load-array
           array-load
           
           savearray
           save-array
           array-save)