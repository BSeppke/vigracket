#lang racket

;;additional requirements
(require vigracket/config)

(require scheme/foreign)
(require racket/list)
(unsafe!)

;;Helper for cvecs: make a cvector with an inital value
(define (make-cvector-init type length val)
  (let ((cvec (make-cvector type length)))
    (do ((i 0 (+ i 1)))
      ((= i length) cvec)
      (cvector-set! cvec i val))))

(define (compatible-cvecs? cvecs)
  (if (<= (length cvecs) 1)
      #t
      (let ((lengths (map cvector-length cvecs)))
        (andmap (curry = (car lengths)) (cdr lengths)))))

;;Helper for cvecs: copying a cvector

(define (fast-cvector-copy-name cvec)
  (let ((layout (ctype->layout (cvector-type cvec))))
    (if (member layout '(uint8 int float double))
        (string->symbol (string-append "vigra_copy_" (symbol->string layout) "_array_c"))
        #f)))


(define (copy-cvector cvec)
  (let ((copy_name (fast-cvector-copy-name cvec)))
    (if (boolean? copy_name)
        ;;no fast copying found - take the slow one...
        (list->cvector (cvector->list cvec) (cvector-type cvec))
        ;;fast variant
        (let* ((fast_func  (get-ffi-obj copy_name vigracket-dylib-path
                                       (_fun (src dest size) :: [src : _cvector]
                                             [dest : _cvector]
                                             [size : _int]
                                             -> (res :  _int))))
               (result (make-cvector (cvector-type cvec) (cvector-length cvec))))
          (case  (fast_func cvec result (cvector-length cvec))
            ((0) result)
            (else (error (string-append "copy-cvector: something went wrong while copying a cvector of size: "
                                        (number->string (cvector-length cvec))
                                        ". the used function for copying was: "  (symbol->string fast-cvector-copy-name)))))
        ))))

; map! a function over cvectors...
(define (cvector-map! func . cvecs)
  (if (null? cvecs)
      '()
      (if (compatible-cvecs? cvecs)
          (let* ((result  (car cvecs))
                 (length (cvector-length  result)))
            (do ((i 0 (+ i 1)))
              ((= i length) result)
              (cvector-set! result i (apply func (map (curryr cvector-ref i) cvecs)))))
          (error "cvector-map!: sizes do not match!"))))

; map a function over cvectors...
(define (cvector-map func . cvecs)
  (if (null? cvecs)
      '()
      (if (compatible-cvecs? cvecs)
          (let* ((result (copy-cvector (car cvecs)))
                 (length (cvector-length result)))
            (do ((i 0 (+ i 1)))
              ((= i length) result)
              (cvector-set! result i (apply func (map (curryr cvector-ref i) cvecs)))))
          (error "cvector-map: sizes do not match!"))))

; foldl function over one vector...
(define (cvector-foldl func cvec seed)
  (let* ((length (cvector-length cvec))
         (reduce_var seed))
    (do ((i 0 (+ i 1)))
      ((= i length) reduce_var)
      (set! reduce_var  (func (cvector-ref cvec i) reduce_var)))))

; foldr function over one vector...
(define (cvector-foldr func cvec seed)
  (let* ((length (cvector-length cvec))
         (reduce_var seed))
    (do ((i 0 (+ i 1)))
      ((= i length) reduce_var)
      (set! reduce_var  (func reduce_var (cvector-ref cvec i))))))

(define cvector-reduce cvector-foldr)

;; Define carray-base as base structure for a multidimensional
;; Array that is based on cvecs
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
  (cvector-ref (carray-data arr) (carray-index pos (carray-dimensions arr))))

;; Setting the item stored at a given position (as list) to a given value
(define (carray-set! arr pos val)
  (cvector-set! (carray-data arr) (carray-index pos (carray-dimensions arr)) val))

;; copy a carray
(define (copy-carray arr)
  (make-carray-base (copy-cvector (carray-data arr)) (carray-dimensions arr)))

;; map! a carray
(define (carray-map! func . carrays)
  (make-carray-base (apply (curry cvector-map! func) (map carray-data carrays)) (carray-dimensions (car carrays))))

;; map a carray
(define (carray-map func . carrays)
  (make-carray-base (apply (curry cvector-map func) (map carray-data carrays)) (carray-dimensions (car carrays))))

;; foldl a carray
(define (carray-foldl func arr seed)
  (cvector-foldl func (carray-data arr) seed))

;; reduce/foldr a carray
(define (carray-foldr func arr seed)
  (cvector-foldr func (carray-data arr) seed))

;; reduce a carray
(define carray-reduce carray-foldr)

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