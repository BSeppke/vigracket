#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require vigracket/convert)

;Dependencies: 
(require racket/gui)
(require racket/draw)

;;#############################################################################
;;####         GUI for showing images. Clicking inside the image            ###
;;####         shows the current pixel values!                              ###
;;#############################################################################
(define (image-value->string image x y)
  (case (image-numbands image)
    ;;for gray-images
    ((1) (real->decimal-string (image-ref image x y 0) 4))
    ;;for rgb-images
    ((3)(string-append "[ R:"
                       (real->decimal-string (image-ref image x y 0) 4)
                       " G:"
                       (real->decimal-string (image-ref image x y 1) 4)
                       " B:"
                       (real->decimal-string (image-ref image x y 2) 4)
                       " ]"))))

;; Defines a class on the fly that overrides the  callback for
;; mouse events in a canvas; you could name it,  if needed
(define image-message%
  (class message%
    (init-field image 
                [status-object #f]) 
    (super-new [label (image->bitmap image)])
    (inherit set-label)
    (define (set-image img) 
      (set! image img)
      (set-label (image->bitmap image)))
    (define (set-status-object stat) 
      (set! status-object stat))
    (set-label (image->bitmap image))
    (define/override (on-subwindow-event sw e)
      (when (not (boolean? status-object))
        (begin
          (define kind (send e moving?))
          (define x (send e get-x))
          (define y (send e get-y))
          (define width  (image-width image))
          (define height (image-height image))
          (when kind
            (if (and (< -1 x width)
                     (< -1 y height))
              ;;show pixel position and value
                (send status-object set-status-text (string-append "Value at pixel (" (number->string x)  ","  (number->string y)   ") is : "
                                                        (image-value->string image x y)))
              ;;errorcase
                (send status-object set-status-text (string-append "Value at pixel ("  (number->string x)  "," (number->string y) ") is not defined!")))))))
    )
)
  

;;Creates a frame, places a canvas inside it  and draws the given pixelarray
;;to the canvas each time the redraw-event is raised.
;;The pixelarray is transformed to a bitmap resp. bitmap_dc to enable
;;painting. Currently, RGB- and Gray-Images are supported
(define (show-image image . windowtitle)
  ;; Make a  frame
  (define frame
    (new frame% [label (if (pair? windowtitle) (car  windowtitle) "vigracket Image Viewer")]))
  ;; Make the drawing area
  (define img-message
    (new image-message% [image image] [parent frame] [status-object frame]))
  ;;enable a status line
  (send frame create-status-line)
  ;; Show the frame
  (send frame show #t))

(define image-show show-image)
(define showimage show-image)

(provide show-image
         image-show 
         showimage
         image-message%)