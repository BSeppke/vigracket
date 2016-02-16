#lang racket

(require vigracket/carray)
(require scheme/foreign)
(require ffi/unsafe)

;######################################################
;##                    IMAGES                        ##
;##--------------------------------------------------##
;## Iamges are represented internally by carrrays    ##
;## of ctype _float and with dimension 2.            ##
;## To ensure an abstraction layer above the carray  ##
;## we add meaningful accessors (e.g. martix-width   ##
;## or image-height for the dimensions accessors.    ##
;## In addition mapping and reduce functions are     ##
;## available both in-place and functional.          ##
;######################################################

(define band_cvalue_type _float)
(define matrix_cvalue_type _double)

(define (float->uint8 f)
  (inexact->exact (max (min (round f) 255) 0)))

(define-fun-syntax _float*
  (syntax-id-rules (_float*)
    [_float* (type: _float pre: (x => (+ 0.0 x)))]))

;########################## General helpers ############################
;pivoting of two-dimensional lists:
; '((st_r_xx st_r_xy st_r_yy)           '((st_r_xx st_g_xx st_b_xx)
;   (st_g_xx st_g_xy st_g_yy)    --->     (st_r_xy st_g_xy st_b_xy)
;   (st_b_xx st_b_xy st_b_yy))            (st_r_yy st_g_yy st_b_yy)
(define (pivot-list xs)
  (if (null? (car xs))
      '()
     (cons (map car xs)
           (pivot-list (map cdr xs)))))

;########################## Band specific functions ############################

; create a band from width/height
; and optional: initial value
(define (make-band width height . init-val)
  (if (pair? init-val)
      (make-carray band_cvalue_type (list width height)(car init-val) )
      (make-carray band_cvalue_type (list width height) )))

; band accessor: data (cvector) of an band
(define (band-data band)
  (carray-data band))

; image accessor: width of a band
(define (band-width band)
  (car (carray-dimensions band)))

; image accessor: height of a band
(define (band-height band)
  (cadr (carray-dimensions band)))

; better accessors - ref for bands
(define (band-ref band x y)
      (cvector-ref (band-data band)
                   (+ x (* (band-width band) y ))))
  
; better accessors - set for bands
(define (band-set! band x y val)
      (cvector-set! (band-data band)
                    (+ x (* (band-width band) y ))
                    val))

; better accessors - unsafe/ref for bands
(define (band-ref/unsafe band x y)
      (cvector-ref/unsafe (band-data band)
                   (+ x (* (band-width band) y ))))
  
; better accessors - unsafe/set for bands
(define (band-set/unsafe! band x y val)
      (cvector-set/unsafe! (band-data band)
                    (+ x (* (band-width band) y ))
                    val))

; copying of bands
(define (copy-band band)
  (copy-carray band))

; band -> image conversion
(define (band->image band)
  (list band))

; band -> matrix conversion
(define (band->matrix band)
  (let* ((mat (make-matrix (band-height band) (band-width band)))
         (foo (band-for-each-index (lambda (x y) (matrix-set! mat y x (band-ref band x y))) band)))
    foo))

; band -> list of lists conversion
(define (band->list band)
  (carray->list band))

; list -> band conversion 
(define (list->band lst)
  (list->carray lst band_cvalue_type))


; map a function (lambda (pixel_intensity 1 .. 2 .. 3 .. n) -> a_new_intensity)
(define band-map carray-map)
  

; same as above but in-place!
; Changes the first given band in the list!
(define band-map! carray-map!)

  
; reduce for bands
(define band-foldl carray-foldl)
(define band-foldr carray-foldr)
(define band-reduce carray-reduce)
  
; apply a function (lambda (x y) -> void)
; to an band in-place!
(define (band-for-each-index func band)
  (let* ((width (band-width band))
         (height (band-height band)))
    (do ((y 0 (+ y 1)))
      ((= y height)  band)
      (do ((x 0 (+ x 1)))
        ((= x width) )
        (func x y)))))

; loop over x and y while a predicate is true:
(define (band-while pred band)
  (let* ((width (band-width band))
         (height (band-height band))
         (found  #f))
    (do ((y 0 (+ y 1)))
        ((or (pair? found) (= y height)) (if (pair? found) found (cons width height)))
      (do ((x 0 (+ x 1)))
          ((or (pair? found) (= x width)) )
        (when (not (pred x y (band-ref band x y)))
            (set! found (cons x y)))))))


;######################### Image specific functions ############################

; create an image from width/height
; and optional: initial color
(define (make-image width height numBands . init-val)
  (if (= numBands 0)
      '() 
      (if (empty? init-val)
          (cons (make-band width height)
                (make-image width height (- numBands 1)))
          (if (= (length init-val) numBands)
              (cons (make-band width height (car init-val))
                (apply (curry make-image width height (- numBands 1)) (cdr init-val)))
              (error "Error in vigracket.helpers.make-image error: if init-vals are used, there have to be as many values as bands")))))
  

; image accessor: width of an image
(define (image-width image)
  (car (carray-dimensions (car image))))

; image accessor: height of an image
(define (image-height image)
  (cadr (carray-dimensions (car image))))

; image accessor: number of bands of an image
(define (image-numbands image)
  (length image))

; better accessors - ref for bands of an image
(define (image-band image band_id)
  (list-ref image band_id))

; better accessors - ref for data of a band of an image
(define (image-data image band_id)
  (band-data (image-band image band_id)))

; better accessors - ref for images
(define (image-ref image x y . band_id)
  (if (null? band_id)
      (letrec ((func (lambda (band_id) 
                       (if (= band_id (image-numbands image))
                           '()
                           (cons (image-ref image x y  band_id) (func (+ band_id 1)))))))
        (func 0))
      (cvector-ref (image-data image (car band_id))
                   (+ x (* (image-width image) y )))))
  
; better accessors - set for images
(define (image-set! image x y band_id . val)
  (if (null? val)
      (letrec ((func (lambda (band_id val_list) 
                       (if (= band_id (image-numbands image))
                           (void)
                           (begin
                             (image-set! image x y band_id (car val_list))
                             (func (+ band_id 1) (cdr val_list)))))))
        (func 0 band_id))
      (cvector-set! (image-data image band_id)
                    (+ x (* (image-width image) y ))
                    (car val))))

; better accessors - unsafe/ref for images
(define (image-ref/unsafe image x y . band_id)
  (if (null? band_id)
      (letrec ((func (lambda (band_id) 
                       (if (= band_id (image-numbands image))
                           '()
                           (cons (image-ref/unsafe image x y  band_id) (func (+ band_id 1)))))))
        (func 0))
      (cvector-ref/unsafe (image-data image (car band_id))
                   (+ x (* (image-width image) y )))))
  
; better accessors - unsafe/set for images
(define (image-set/unsafe! image x y band_id . val)
  (if (null? val)
      (letrec ((func (lambda (band_id val_list) 
                       (if (= band_id (image-numbands image))
                           (void)
                           (begin
                             (image-set/unsafe! image x y band_id (car val_list))
                             (func (+ band_id 1) (cdr val_list)))))))
        (func 0 band_id))
      (cvector-set/unsafe! (image-data image band_id)
                    (+ x (* (image-width image) y ))
                    (car val))))

; copying of images
(define (copy-image image)
  (map copy-band image))

; image -> channel bands
(define (image->red-band image)
  (if (= (image-numbands image) 3)
      (image-band image 0)
      (error "Error in vigracket.helpers.image->red-band: Band extraction is only allowed for 3-channel RGB images")))

(define (image->green-band image)
  (if (= (image-numbands image) 3)
      (image-band image 1)
      (error "Error in vigracket.helpers.image->green-band: Band extraction is only allowed for 3-channel RGB images")))

(define (image->blue-band image)
  (if (= (image-numbands image) 3)
      (image-band image 2)
      (error "Error in vigracket.helpers.image->blue-band: Band extraction is only allowed for 3-channel RGB images")))


; image -> channel images
(define (image->red image)
  (list (image->red-band image)))

(define (image->green image)
  (list (image->green-band image)))

(define (image->blue image)
  (list (image->blue-band image)))

; image -> list of lists conversion
(define (image->list image)
  (map band->list image))

; list -> image conversion 
(define (list->image lst)
  (map list->band lst))


; map a function (lambda (pixel_intensity 1 .. 2 .. 3 .. n) -> a_new_intensity)
;; band-wise to multiple images
(define (image-map func . images)
  (if (null? (car images))
      '()
      (cons (apply (curry band-map func) (map car images))
            (apply (curry image-map func) (map cdr images)))))

; same as above but in-place!
; Changes the first given band in the list!
(define (image-map! func . images)
  (if (null? (car images))
      '()
      (cons (apply (curry band-map! func) (map car images))
            (apply (curry image-map! func) (map cdr images)))))

  
; reduce for images
(define (image-reduce func image seed)
  (map (lambda (band) (band-reduce func band seed)) image))
  
; apply a function (lambda (x y band_id) -> void)
; to an band in-place!
(define (image-for-each-index func image)
  (let* ((width (image-width image))
         (height (image-height image))
         (numBands (image-numbands image)))
    (do ((band_id 0 (+ band_id 1)))
      ((= band_id numBands) image)
      (do ((y 0 (+ y 1)))
        ((= y height) )
        (do ((x 0 (+ x 1)))
          ((= x width) )
          (func x y band_id))))))

; apply a function (lambda (x y '(intensity_1 ... intensity_n)) -> void)
; to an band in-place!
(define (image-for-each-pixel func image)
  (let* ((width (image-width image))
         (height (image-height image))
         (numBands (image-numbands image)))
    (do ((y 0 (+ y 1)))
        ((= y height) image)
        (do ((x 0 (+ x 1)))
          ((= x width) )
          (func x y (image-ref/unsafe image x y))))))
  
 
; loop over x and y of an image (pixel-wise) while a predicate is true:
(define (image-while pred image)
  (let* ((width (image-width image))
         (height (image-height image))
         (numBands (image-numbands image))
         (found  #f))
    (do ((y 0 (+ y 1)))
        ((or (pair? found) (= y height)) (if (pair? found) found (cons width height)))
      (do ((x 0 (+ x 1)))
          ((or (pair? found) (= x width)) )
        (when (not (pred x y (image-ref/unsafe image x y)))
          (set! found (cons x y)))))))

;######################################################
;##                    MATRICES                      ##
;##--------------------------------------------------##
;## Matrices are represented internally by carrrays  ##
;## of ctype _double and with dimension 2.           ##
;## To ensure an abstraction layer above the carray  ##
;## we add meaningful accessors (e.g. martix-cols    ##
;## or matrix-rows for the dimensions accessors.     ##
;## In addition a  matrix multiplication is avail.   ##
;######################################################


; creation of an image from width/height 
; and optional: initial value
(define (make-matrix rows cols . init-val)
  (if (pair? init-val)
      (make-carray matrix_cvalue_type (list rows cols)(car init-val) )
      (make-carray matrix_cvalue_type (list rows cols) )))
  
; matrix-accessors - rows of matrix
(define (matrix-rows mat)
  (car (carray-dimensions mat)))

; matrix-accessors - columns of matrix
(define (matrix-cols mat)
  (cadr (carray-dimensions mat)))
  
; matrix-accessors - data inside matrix
(define (matrix-data matrix)
  (carray-data matrix))
  
; better accessors - ref
(define (matrix-ref mat r c)
  (cvector-ref (carray-data mat) (+ r (* (matrix-rows mat) c ))))
  
; better accessors - set
(define (matrix-set! mat r c val)
  (cvector-set! (carray-data mat) (+ r (* (matrix-rows mat) c )) val))
 
; copying of matrices
(define (copy-matrix mat)
  (copy-carray mat))
  
; matrix -> list of lists conversion
(define (matrix->list mat)
  (carray->list mat))

; list -> matrix conversion
(define (list->matrix lst)
  (list->carray lst matrix_cvalue_type))

; multiplication of two matrices 
(define (matrix-mult mat1 mat2)
  (if (and (= (matrix-rows mat1)
              (matrix-cols mat2))
           (= (matrix-cols mat1)
              (matrix-rows mat2)))
        (let ((new-matrix (make-matrix (matrix-rows mat2) (matrix-cols mat1) 0.0)))
          (do ((c 0 (+ c 1)))
            ((= c (matrix-cols mat1)) new-matrix)
            (do ((r 0 (+ r 1)))
              ((= r  (matrix-rows mat2)))
              (do ((i 0 (+ i 1)))
                ((= i (matrix-rows mat1)))
                (matrix-set! new-matrix r c (+ (matrix-ref new-matrix r c)
                                               (* (matrix-ref mat1 i c)
                                                  (matrix-ref mat2 r i))))))))
        (error "Error in vigracket.helpers.matrix-mult: Matrix sizes do not match")))


(provide (all-defined-out))