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

;; Defines a class on the fly that overrides the callback for
;; mouse events in a canvas; you could name it,  if needed
(define image-canvas%
  (class canvas%
    (init-field image [racket_img #f]) 
    (define/override (on-paint)
      (begin
          (when (boolean? racket_img)
            (set! racket_img (image->racket-image image)))
          (send (send this get-dc) draw-bitmap racket_img 0 0)))
    (define/override (on-event e) 
        (begin
          (when (send e moving?)
            (let-values (((off_x off_y) (send this get-view-start)))
              (let* ((parent (send this get-parent))
                     (x (+ (send e get-x) off_x))
                     (y (+ (send e get-y) off_y))
                     (width  (image-width image))
                     (height (image-height image)))
                (if (and (< -1 x width)
                         (< -1 y height))
                    ;;show pixel position and value
                    (send parent set-status-text (string-append "Value at pixel (" (number->string x)  ","  (number->string y)   ") is : "
                                                              (image-value->string image x y)))
                    ;;errorcase
                    (send parent set-status-text (string-append "Value at pixel ("  (number->string x)  "," (number->string y) ") is not defined!"))))))))
    (super-new [style '(vscroll hscroll resize-corner)])))
  

;;Creates a frame, places a canvas inside it  and draws the given pixelarray
;;to the canvas each time the redraw-event is raised.
;;The pixelarray is transformed to a bitmap resp. bitmap_dc to enable
;;painting. Currently, RGB- and Gray-Images are supported
(define (show-image image . windowtitle)
  ;; Make a  frame
  (define frame
    (new frame% 
         [label  (if (pair? windowtitle) (car  windowtitle) "vigracket Image Viewer")]))
  ;; Make the drawing area
  (define canvas
    (new image-canvas% [image image] [parent frame]))
  ;; Adjust it
  (send canvas min-client-width (image-width image))
  (send canvas min-client-height (image-height image))
  (send canvas horiz-margin 0)
  (send canvas vert-margin 0)
  (send canvas init-auto-scrollbars (image-width image) (image-height image) 0.0 0.0)
  ;; Enable a status line
  (send frame create-status-line)
  ;; Show the frame
  (send frame show #t))

(define image-show show-image)
(define showimage show-image)

(provide show-image
         image-show 
         showimage
         image-canvas%)