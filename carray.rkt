#lang racket

;;additional requirements
(require vigracket/config)

(require scheme/foreign)
(require racket/list)
(unsafe!)

;;Helper for cvectors: make a cvector with an inital value
(define (make-cvector-init type length val)
  (let ((cvec (make-cvector type length)))
    (do ((i 0 (+ i 1)))
      ((= i length) cvec)
      (cvector-set! cvec i val))))

;;Helper for cvectors: copying a cvector

(define (fast-cvector-copy-name cvec)
  (case (cvector-type cvec)
    ((_uint8)  'vigra_copy_uint8_array)
    ((_int)    'vigra_copy_int_array)
    ((_float)  'vigra_copy_float_array)
    ((_double) 'vigra_copy_double_array)
    (else      #f)))


(define (copy-cvector cvec)
  (let ((copy_name (fast-cvector-copy-name cvec)))
    (if (boolean? copy_name)
        ;;no fast copying found - take the slow one...
        (list->cvector (cvector->list cvec) (cvector-type cvec))
        ;;fast variant
        ((get-ffi-obj copy_name vigracket-dylib-path
               (_fun (src dest size) :: [src : _cvector]
                     [dest : _cvector]
                     [size : _int]
                     -> (res :  _int))) cvec))))


;; Define carray-base as base structure for a multidimensional
;; Array that is based on cvectors
;; Note that the name is carray-base as we want to write our
;; own constructor make-carray and other functions!
(define-struct carray-base 
  (data
   dimensions))
  
;; Constructor for carrays (with given type, a list containing
;; the dimensions of the resulting array, and an initial value
;; for each element.
(define (make-carray type dimensions . init-val)
  (if (null? init-val)
      (make-carray-base (make-cvector type (apply * dimensions))
                        dimensions)
      (make-carray-base (make-cvector-init type (apply * dimensions) (car init-val))
                   dimensions)))

;; Accessor for the cvector inside the carray representation
(define (carray-data arr) 
  (carray-base-data arr))

;; Accessor for the dimensions of the carray representation
(define (carray-dimensions arr) 
  (carray-base-dimensions arr))

;; Accessor for the type of the carray
(define (carray-type arr) 
  (cvector-type (carray-data arr)))

;; carray-ref helper: Converts a given position and dimension list into
;; a linescan index of that position in the cvector
;; Example: (carray-index '(1 1 1) '(10 20 30))  
;;             -> 1+ 1*10 + 1*(10*20) = 211
(define (carray-index pos dimensions)
  (letrec ((func   (lambda (pos dimensions akk akk2)
                     (if (null? pos)
                         akk
                         (func (cdr pos) 
                               (cdr dimensions)
                               (+ (* (car pos) akk2)
                                  akk)
                               (* (car dimensions) akk2))))))
    (func pos dimensions 0 1)))

;; Accessor for the item stored at a given position (as list)
(define (carray-ref arr pos)
  (cvector-ref (carray-data arr) (carray-index pos(carray-dimensions arr))))

;; Setting the item stored at a given position (as list) to a given value
(define (carray-set! arr pos val)
  (cvector-set! (carray-data arr) (carray-index pos (carray-dimensions arr)) val))

;; copy a carray
(define (copy-carray arr)
  (make-carray-base (copy-cvector (carray-data arr)) (carray-dimensions arr)))

;list->carray helpers: extract the dimension of a hierarchical array
(define (list->carray-dimensions xs)
  (letrec ((func (lambda (x)
                   (if (list? x)
                       (cons (length x) (func (car x)))
                       '()))))
    (reverse (func xs))))

;;list->carray: converts a hierarchical list into a multidimensional array
(define (list->carray xs type) 
  (make-carray-base (list->cvector (flatten xs) type)
                    (list->carray-dimensions xs)))
  

;; carray->list helpers: convert a carray into a flat list
(define (carray->flatlist arr) 
 (cvector->list (carray-data arr)))

;; carray->list helpers: convert a flat list to a splitted one
;; according to a given list of parts!
(define (list-split xs parts)
  (if (null? parts)
      xs
      (if (= (car parts) 0)
          '()
          (let* ((len (length parts))
                 (func (if (= 1 (length parts))
                           append
                           cons)))
              (func (list-split (take xs (/ (length xs) (car parts)))
                                (cdr parts))
                    (list-split (drop xs (/ (length xs) (car parts)))
                                (cons (- (car parts) 1) (cdr parts))))))))


;;converts a multidimensional array to a hierarchical list
(define (carray->list arr) 
 (list-split (carray->flatlist arr) (reverse (carray-dimensions arr))))
  
  
;;For testing purpose
;(define xs '(((0.0 1.0 2.0 3.0)
;             (4.0 5.0 6.0 7.0)
;             (8.0 9.0 10.0 11.0))
;             
;             ((0.0 1.0 2.0 3.0)
;              (4.0 5.0 6.0 7.0)
;              (8.0 9.0 10.0 11.0))
;             
;             ((12.0 13.0 14.0 15.0)
;              (16.0 17.0 18.0 19.0)
;              (20.0 21.0 22.0 23.0))))
;
;(carray->list (list->carray xs _float))

(provide (all-defined-out))