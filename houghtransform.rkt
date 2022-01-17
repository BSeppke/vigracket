#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)

(define-cpointer-type _houghlines)
(define-cpointer-type _houghcircles)

;###############################################################################
;###################        Hough Transform (lines)         ####################

(define vigra_houghtransform_lines_c
  (get-ffi-obj 'vigra_houghtransform_lines_c vigracket-dylib-path
               (_fun (img_vector width height angles min_supporters) :: [img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     [angles : _int]
                     [min_supporters : _int]
                     -> (res :  _houghlines))))

(define vigra_houghtransform_get_line_count_c
  (get-ffi-obj 'vigra_houghtransform_get_line_count_c vigracket-dylib-path
               (_fun (hough_lines) :: [hough_lines : _houghlines]
                     -> (res :  _int))))

(define vigra_houghtransform_get_lines_c
  (get-ffi-obj 'vigra_houghtransform_get_lines_c vigracket-dylib-path
               (_fun (hough_lines img_vector width height) :: [hough_lines : _houghlines][img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define vigra_houghtransform_delete_lines_c
  (get-ffi-obj 'vigra_houghtransform_delete_lines_c vigracket-dylib-path
               (_fun (hough_lines) :: [hough_lines : _houghlines]
                     -> (res :  _int))))

(define (houghtransform-lines-band band angles min_supporters)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (houghlines (vigra_houghtransform_lines_c (band-data band) width height angles min_supporters)))
    (if (boolean? houghlines)
      (error "Error in vigracket.houghtransform-lines: Nullptr returned!")
      (let* ((width 3)
             (height (vigra_houghtransform_get_line_count_c houghlines))
             (resBand (make-band width height))
             (res0 (vigra_houghtransform_get_lines_c houghlines (band-data resBand) width height))
             (res1 (vigra_houghtransform_delete_lines_c houghlines)))
        (if (> 0 (+ res0 res1))
            (error "Error in vigracket.houghtransform-lines: Error while extracting or deleting the hough lines!")
            (list resBand))))))

(define (houghtransform-lines img angles min_supporters)
  (if (= (length img) 1)
      (houghtransform-lines-band (car img) angles min_supporters)
      (error "Error in vigracket.houghtransform-lines: Only applicable to single band images.")))

            
;###############################################################################
;###################        Hough Transform (circles)         ####################

(define vigra_houghtransform_circles_c
  (get-ffi-obj 'vigra_houghtransform_circles_c vigracket-dylib-path
               (_fun (img_vector width height min_radius max_radius min_supporters) :: [img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     [min_radius : _int]
                     [max_radius : _int]
                     [min_supporters : _int]
                     -> (res :  _houghcircles))))

(define vigra_houghtransform_get_circle_count_c
  (get-ffi-obj 'vigra_houghtransform_get_circle_count_c vigracket-dylib-path
               (_fun (hough_circles) :: [hough_circles : _houghcircles]
                     -> (res :  _int))))

(define vigra_houghtransform_get_circles_c
  (get-ffi-obj 'vigra_houghtransform_get_circles_c vigracket-dylib-path
               (_fun (hough_circles img_vector width height) :: [hough_circles : _houghcircles][img_vector : _cvector]
                     [width : _int]
                     [height : _int]
                     -> (res :  _int))))

(define vigra_houghtransform_delete_circles_c
  (get-ffi-obj 'vigra_houghtransform_delete_circles_c vigracket-dylib-path
               (_fun (hough_circles) :: [hough_circles : _houghcircles]
                     -> (res :  _int))))

(define (houghtransform-circles-band band min_radius max_radius min_supporters)
  (let* ((width  (band-width  band))
	 (height (band-height band))
	 (houghcircles (vigra_houghtransform_circles_c (band-data band) width height min_radius max_radius min_supporters)))
    (if (boolean? houghcircles)
      (error "Error in vigracket.houghtransform-circles: Nullptr returned!")
      (let* ((width 4)
             (height (vigra_houghtransform_get_circle_count_c houghcircles))
             (resBand (make-band width height))
             (res0 (vigra_houghtransform_get_circles_c houghcircles (band-data resBand) width height))
             (res1 (vigra_houghtransform_delete_circles_c houghcircles)))
        (if (> 0 (+ res0 res1))
            (error "Error in vigracket.houghtransform-circles: Error while extracting or deleting the hough circles!")
            (list resBand))))))
               
(define (houghtransform-circles img min_radius max_radius min_supporters)
  (if (= (length img) 1)
      (houghtransform-circles-band (car img) min_radius max_radius min_supporters)
      (error "Error in vigracket.houghtransform-circles: Only applicable to single band images.")))
              
        
           
(provide
           houghtransform-lines
           houghtransform-circles
           )