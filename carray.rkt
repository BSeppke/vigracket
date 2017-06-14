#lang racket

;;additional requirements
(require vigracket/config)

(require ffi/unsafe)
(require ffi/cvector)

(define-syntax define*
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin (provide name) (define (name . args) body ...))]
    [(_ name expr)
     (begin (provide name) (define name expr))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;                                                                          ;;;
;;;              ADDITIONS TO THE CVECTOR STRUCTURE OF RACKET'S FFI          ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check for compatible types and sizes
(define (compatible-cvecs? . cvecs)
  (if (<= (length cvecs) 1)
      #t
      (let ((lengths (map cvector-length cvecs))
            (types (map cvector-type cvecs)))
        (and (andmap (curry = (car lengths)) (cdr lengths))
             (andmap (curry equal? (car types)) (cdr types))))))

;; unsafe accessors to cvectors - need for fast traversal (if you are sure, that your index is correct)        
(define* (cvector-ref/unsafe v i)
  (ptr-ref (cvector-ptr v) (cvector-type v) i))

;; unsafe accessors to cvectors - need for fast traversal (if you are sure, that your index and value is correct)
(define* (cvector-set/unsafe! v i x)
  (ptr-set! (cvector-ptr v) (cvector-type v) i x))

;; fast copying of cvectors
(define (copy-cvector cvec)
  (let* ((len  (cvector-length cvec))
         (type (cvector-type cvec))
         (result (make-cvector type len))
         (foo (memcpy   (cvector-ptr result) 0
                        (cvector-ptr cvec)   0
                        len
                        type)))
     result))
      
;; map! a function over cvectors - (over)writes the result in the first given vector
(define (cvector-map! func . cvecs)
  (if (null? cvecs)
      #f
      (if (apply compatible-cvecs? cvecs)
          (let* ((result  (car cvecs))
                 (length (cvector-length result)))
            (do ((i 0 (+ i 1)))
              ((= i length) result)
              (cvector-set/unsafe! result i (apply func (map (curryr cvector-ref/unsafe i) cvecs)))))
          (error "cvector-map!: sizes/types do not match!"))))

;; map a function over cvectors returns the result in a newly allocated cvector
(define (cvector-map func . cvecs)
  (if (null? cvecs)
      #f
      (apply (curry cvector-map! func) (cons (copy-cvector (car cvecs)) (cdr cvecs)))))

;; foldl function over one cvector...
(define (cvector-foldl func seed cvec)
  (let* ((length (cvector-length cvec))
         (reduce_var seed))
    (do ((i 0 (+ i 1)))
      ((= i length) reduce_var)
      (set! reduce_var  (func (cvector-ref/unsafe cvec i) reduce_var)))))

;; foldr function over one cvector...
(define (cvector-foldr func seed cvec)
  (let* ((length (cvector-length cvec))
         (reduce_var seed))
    (do ((i 0 (+ i 1)))
      ((= i length) reduce_var)
      (set! reduce_var  (func reduce_var (cvector-ref/unsafe cvec i))))))

;; reduce uses other argument order
(define (cvector-reduce func cvec seed)
  (cvector-foldr func seed cvec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;                                                                          ;;;
;;;          INTRODUCTION OF AN ARRAY TYPE "CARRRAY" BASED ON CVECTORS       ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The C-Array that is based on a cvector.
(define-struct carray
  (data
   dimensions))
  

;; Constructor for carrays (with given type, a list containing
;; the dimensions of the resulting array
(provide carray? carray-data carray-dimensions 
         (rename-out [allocate-carray make-carray]))

(define (allocate-carray type dimensions)
  (make-carray (make-cvector type (apply * dimensions))
               dimensions))

;; Accessor for the type of the carray
(define* (carray-type arr) 
  (cvector-type (carray-data arr)))

;; carray-ref helper: Converts a given position and dimension list into
;; a linescan index of that position in the cvector
;; Example: (carray-index '(1 1 1) '(10 20 30))  
;;             -> 1+ 1*10 + 1*(10*20) = 211
(define* (carray-index pos dimensions)
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
(define* (carray-ref arr pos)
  (cvector-ref (carray-data arr) (carray-index pos (carray-dimensions arr))))

;; Setting the item stored at a given position (as list) to a given value
(define* (carray-set! arr pos val)
  (cvector-set! (carray-data arr) (carray-index pos (carray-dimensions arr)) val))

;; Accessor for the item stored at a given position (as list)
(define* (carray-ref/unsafe arr pos)
  (cvector-ref/unsafe (carray-data arr) (carray-index pos (carray-dimensions arr))))

;; Setting the item stored at a given position (as list) to a given value
(define* (carray-set/unsafe! arr pos val)
  (cvector-set/unsafe! (carray-data arr) (carray-index pos (carray-dimensions arr)) val))

;; copy a carray
(define* (copy-carray arr)
  (make-carray (copy-cvector (carray-data arr)) (carray-dimensions arr)))

;; map! a carray
(define* (carray-map! func . carrays)
  (make-carray (apply (curry cvector-map! func) (map carray-data carrays)) (carray-dimensions (car carrays))))

;; map a carray
(define* (carray-map func . carrays)
  (make-carray (apply (curry cvector-map func) (map carray-data carrays)) (carray-dimensions (car carrays))))

;; foldl a carray
(define* (carray-foldl func seed arr)
  (cvector-foldl func seed (carray-data arr)))

;; reduce a carray
(define* (carray-foldr func seed arr)
  (cvector-foldr func seed (carray-data arr)))

;; reduce a carray
(define* (carray-reduce func arr seed)
  (cvector-reduce func (carray-data arr) seed))

;list->carray helpers: extract the dimension of a hierarchical array
(define* (list->carray-dimensions xs)
  (letrec ((func (lambda (x)
                   (if (list? x)
                       (cons (length x) (func (car x)))
                       '()))))
    (reverse (func xs))))

;;list->carray: converts a hierarchical list into a multidimensional array
(define* (list->carray xs type) 
  (make-carray (list->cvector (flatten xs) type)
                    (list->carray-dimensions xs)))
  

;; carray->list helpers: convert a carray into a flat list
(define* (carray->flatlist arr) 
 (cvector->list (carray-data arr)))

;; carray->list helpers: convert a flat list to a splitted one
;; according to a given list of parts!
(define* (list-split xs parts)
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
(define* (carray->list arr) 
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