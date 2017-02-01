#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require vigracket/convert)

;Dependencies: 
(require racket/gui)
(require racket/draw)
(define (rex r)
  (inexact->exact (round r)))

;;#############################################################################
;;####         GUI for showing images. Clicking inside the image            ###
;;####         shows the current pixel values!                              ###
;;#############################################################################
(define (image-value->string image x y)
  (let ((nx (rex x))
        (ny (rex y)))
    (case (image-numbands image)
      ;;for gray-images
      ((1) (real->decimal-string (image-ref image nx ny 0) 4))
      ;;for rgb-images
      ((3)(string-append "[ R:"
                         (real->decimal-string (image-ref image nx ny 0) 4)
                         " G:"
                         (real->decimal-string (image-ref image nx ny 1) 4)
                         " B:"
                         (real->decimal-string (image-ref image nx ny 2) 4)
                         " ]"))
      ;;for rgba-images
      ((3)(string-append "[ R:"
                         (real->decimal-string (image-ref image nx ny 0) 4)
                         " G:"
                         (real->decimal-string (image-ref image nx ny 1) 4)
                         " B:"
                         (real->decimal-string (image-ref image nx ny 2) 4)
                         " A:"
                         (real->decimal-string (image-ref image nx ny 3) 4)
                         " ]")))))

;; Defines a class on the fly that overrides the callback for
;; mouse events in a canvas; you could name it,  if needed
(define image-canvas%
  (class canvas%
    (init-field image [racket_img #f]) 
    ;; Paint Event Handler -> simply draws the image to the device context (dc%)
    (define/override (on-paint)
      (begin
          (when (boolean? racket_img)
            (set! racket_img (image->racket-image image)))
          (send (send this get-dc) draw-bitmap racket_img 0 0)))
    ;; Key (and mouse wheel) Handler:
    ;  Arrow-Keys: move canvas (if possible)
    ;  Mouse wheel down or Ctrl+"+": Zoom in (+10%)
    ;  Mouse wheel up or Ctrl+"-": Zoom out (-10%)
    ;  Ctrl+"0": Reset Zoom to 100%
    (define/override (on-char e) 
      (let*-values (((ctrl)  (send e get-control-down))
                    ((key)   (send e get-key-code))
                    ((dc)    (send this get-dc))
                    ((s_x s_y) (send dc get-scale))
                    ((off_x off_y) (send this get-view-start))
                    ((cli_w cli_h) (send this get-client-size))
                    ((scr_x scr_y) (values (/ off_x (- (* (image-width image)  s_x) cli_w))
                                           (/ off_y (- (* (image-height image) s_y) cli_h))))
                    ((w h) (values (image-width image) (image-height image))))
        (when (or (and ctrl (eq? key #\+))
                  (eq? key 'wheel-down))  
          (begin 
            (send dc set-scale (* s_x 1.1)(* s_y 1.1))
            (send this init-auto-scrollbars
                  (rex (* w (* s_x 1.1)))
                  (rex (* h (* s_y 1.1)))
                  scr_x
                  scr_y)
              (send this refresh)))
          (when (or (and ctrl (eq? key #\-))
                    (eq? key 'wheel-up)) 
            (begin 
              (send dc set-scale (/ s_x 1.1)(/ s_y 1.1))
              (send this init-auto-scrollbars
                    (rex (* w (/ s_x 1.1)))
                    (rex (* h (/ s_y 1.1)))
                    scr_x
                    scr_y)
              (send this refresh)))
          (when (and ctrl (eq? key #\0)) 
            (begin 
              (send dc set-scale 1.0 1.0)
              (send this init-auto-scrollbars
                    w
                    h
                    scr_x
                    scr_y)
              (send this refresh)))
          (when (eq? key 'up)
            (send this scroll #f (max 0.0 (- scr_y (/ 1.0 s_y)))))
          (when (eq? key 'down)
            (send this scroll #f (min 1.0 (+ scr_y (/ 1.0 s_y)))))
          (when (eq? key 'left)
            (send this scroll (max 0.0 (- scr_x (/ 1.0 s_x))) #f))
          (when (eq? key 'right)
            (send this scroll (min 1.0 (+ scr_x (/ 1.0 s_x))) #f))))
    ;; Mouse move event handler
    (define/override (on-event e) 
        (begin
          (when (send e moving?)
            (let-values (((off_x off_y) (send this get-view-start)))
              (let* ((parent (send this get-parent))
                     (dc     (send this get-dc))
                     (width  (image-width image))
                     (height (image-height image)))
                (let-values (((s_x s_y) (send dc get-scale)))
                  (let* ((x (/ (+ (send e get-x) off_x) s_x))
                         (y (/ (+ (send e get-y) off_y) s_y)))
                  (if (and (<= 0 x (- width  1))
                           (<= 0 y (- height 1)))
                    ;;show pixel position and value
                      (send parent set-status-text (string-append "Value at pixel (" (real->decimal-string x 2)  ","  (real->decimal-string y 2)   ") is : "
                                                                  (image-value->string image x y)))
                    ;;errorcase
                      (send parent set-status-text (string-append "Value at pixel ("  (real->decimal-string x 2)  "," (real->decimal-string y 2) ") is not defined!"))))))))))
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