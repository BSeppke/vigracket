#lang racket

(require racket/draw)

(require (only-in 2htdp/image
                  (image? 2htdp-image?)  
                  (image-width 2htdp-image-width)
                  (image-height 2htdp-image-height)))
(require (only-in mrlib/image-core render-image))

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

(define (bytes->cvector bytes)
  (list->cvector (bytes->list bytes) _uint8))

(define (cvector->bytes cvector)
  (list->bytes (cvector->list cvector)))

(define vigra_convert_grayband_to_argb_c
  (get-ffi-obj 'vigra_convert_grayband_to_argb_c vigracket-dylib-path
               (_fun (img_vector argb_vector width height) :: [img_vector : _cvector]
                     [argb_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (grayimage->argb-bytes image)
  (let* ((band   (car image))
         (width  (band-width  band))
         (height (band-height band))
         (argb_bytes_cvector (make-cvector _uint8 (* width height 4)))
         (foo   (vigra_convert_grayband_to_argb_c (band-data band) argb_bytes_cvector width height)))
    (case foo
      ((0) (cvector->bytes argb_bytes_cvector))
      ((1) (error "Error in vigracket.convert.grayimage->argb-bytes. Conversion failed!")))))

(define vigra_convert_rgbbands_to_argb_c
  (get-ffi-obj 'vigra_convert_rgbbands_to_argb_c vigracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b argb_vector width height) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [argb_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (rgbimage->argb-bytes image)
  (let* ((r_band   (first  image))
         (g_band   (second image))
         (b_band   (third  image))
         (width  (band-width  r_band))
         (height (band-height r_band))
         (argb_bytes_cvector (make-cvector _uint8 (* width height 4)))
         (foo   (vigra_convert_rgbbands_to_argb_c (band-data r_band) (band-data g_band) (band-data b_band) argb_bytes_cvector width height)))
    (case foo
      ((0) (cvector->bytes argb_bytes_cvector))
      ((1) (error "Error in vigracket.convert.rgbimage->argb-bytes. Conversion failed!")))))

(define (image->bitmap img [bitmap null])
  (let* ((w  (image-width img))
         (h  (image-height img))
         (bm (if (empty? bitmap)
                 (make-object bitmap% w h #f #|not monochrome|# #t #| alpha channel|#)
                 bitmap)))
    (case (image-numbands img)
      ((1) ;;Grayscale
       (begin
         (send bm set-argb-pixels 0 0 w h (grayimage->argb-bytes img))
         bm))
      ((3) ;;RedGreenBlue
       (begin
         (send bm set-argb-pixels 0 0 w h (rgbimage->argb-bytes img))
         bm))
      (else (error "Error in vigracket.convert.image->bitmap. Conversion only defined for rgb or grayscale images!")))))


(define vigra_convert_argb_to_grayband_c
  (get-ffi-obj 'vigra_convert_argb_to_grayband_c vigracket-dylib-path
               (_fun (argb_vector img_vector width height) :: [argb_vector : _cvector]
                     [img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (argb-bytes->grayimage bytes image)
  (case  (vigra_convert_argb_to_grayband_c (bytes->cvector bytes) (image-data image 0) (image-width image) (image-height image))
    ((0) image)
    ((1) (error "Error in vigracket.convert.argb-bytes->grayimage. Conversion failed!"))))

(define vigra_convert_argb_to_rgbbands_c
  (get-ffi-obj 'vigra_convert_argb_to_rgbbands_c vigracket-dylib-path
               (_fun (argb_vector img_vector_r img_vector_g img_vector_b width height) :: [argb_vector : _cvector]
                     [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define (argb-bytes->rgbimage bytes image)
  (case (vigra_convert_argb_to_rgbbands_c  (bytes->cvector bytes) (image-data image 0) (image-data image 1) (image-data image 2) (image-width image) (image-height image))
    ((0) image)
    ((1) (error "Error in vigracket.convert.argb-bytes->grayimage. Conversion failed!"))))

(define (2htdp-image->bitmap 2htdp-image [bitmap null])
  (let* ((bm (if (empty? bitmap)
                 (make-object bitmap% (2htdp-image-width 2htdp-image) (2htdp-image-height 2htdp-image))
                 bitmap))
         (bitmap-dc (make-object bitmap-dc% bm))
         (foo (render-image 2htdp-image bitmap-dc 0 0)))
    bm))

(define (bitmap->grayimage bitmap [image null])
  (cond ((is-a? bitmap bitmap%)   (let* ((w (send bitmap get-width))
                                         (h (send bitmap get-height))
                                         (img  (if (empty? image)
                                                   (make-image w h 1)
                                                   image))
                                         (bytes (make-bytes (* w h 4)))
                                         (foo   (send bitmap get-argb-pixels 0 0 w h bytes)))
                                    (argb-bytes->grayimage bytes img)))
        ((2htdp-image? bitmap)    (bitmap->grayimage (2htdp-image->bitmap bitmap) image))
        (else (error "vigracket.convert.bitmap->grayimage: Only Racket bitmap% objects and 2htdp/images can be converted to vigracket images!"))))

(define (bitmap->rgbimage bitmap [image null])
  (cond ((is-a? bitmap bitmap%) (let* ((w (send bitmap get-width))
                                       (h (send bitmap get-height))
                                       (img  (if (empty? image)
                                                 (make-image w h 3)
                                                 image))
                                       (bytes (make-bytes (* w h 4)))
                                       (foo   (send bitmap get-argb-pixels 0 0 w h bytes)))
                                  (argb-bytes->rgbimage bytes w h img)))
        ((2htdp-image? bitmap)    (bitmap->rgbimage (2htdp-image->bitmap bitmap) image))
        (else (error "vigracket.convert.bitmap->rgbimage: Only Racket bitmap% objects and 2htdp/images can be converted to vigracket images!"))))

(define bitmap->image bitmap->rgbimage)

(define image->plt-image image->bitmap)
(define plt-image->image bitmap->rgbimage)

(define image->racket-image image->plt-image)
(define racket-image->image plt-image->image)

(provide image->bitmap
         bitmap->image
         bitmap->grayimage
         bitmap->rgbimage
         plt-image->image
         image->plt-image
         racket-image->image
         image->racket-image)