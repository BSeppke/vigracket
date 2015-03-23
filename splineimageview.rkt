#lang racket

(require vigracket/config)
(require vigracket/helpers)
(require scheme/foreign)
(unsafe!)


(define-cpointer-type _splineimageview1)
(define-cpointer-type _splineimageview2)
(define-cpointer-type _splineimageview3)
(define-cpointer-type _splineimageview4)
(define-cpointer-type _splineimageview5)
  
(define vigra_create_splineimageview1_c
    (get-ffi-obj 'vigra_create_splineimageview1_c  vigracket-dylib-path
                 (_fun  (img_vector width height) ::[img_vector : _cvector]
                        [width : _int]
                        [height : _int]
                      -> [siv_ptr : _splineimageview1])))

(define vigra_create_splineimageview2_c
    (get-ffi-obj 'vigra_create_splineimageview2_c  vigracket-dylib-path
                 (_fun  (img_vector width height) ::[img_vector : _cvector]
                        [width : _int]
                        [height : _int]
                        -> [siv_ptr : _splineimageview2])))

(define vigra_create_splineimageview3_c
    (get-ffi-obj 'vigra_create_splineimageview3_c  vigracket-dylib-path
                 (_fun  (img_vector width height) ::[img_vector : _cvector]
                        [width : _int]
                        [height : _int]
                        -> [siv_ptr : _splineimageview3])))

(define vigra_create_splineimageview4_c
    (get-ffi-obj 'vigra_create_splineimageview4_c  vigracket-dylib-path
                 (_fun  (img_vector width height) ::[img_vector : _cvector]
                        [width : _int]
                        [height : _int]
                        -> [siv_ptr : _splineimageview4])))

(define vigra_create_splineimageview5_c
    (get-ffi-obj 'vigra_create_splineimageview5_c  vigracket-dylib-path
                 (_fun  (img_vector width height) ::[img_vector : _cvector]
                        [width : _int]
                        [height : _int]
                        -> [siv_ptr : _splineimageview5])))

(define vigra_delete_splineimageview1_c
    (get-ffi-obj 'vigra_delete_splineimageview1_c  vigracket-dylib-path
                 (_fun [siv_ptr : _splineimageview1]
                       -> [res : _int]
                      )))

(define vigra_delete_splineimageview2_c
    (get-ffi-obj 'vigra_delete_splineimageview2_c  vigracket-dylib-path
                 (_fun [siv_ptr : _splineimageview2]
                       -> [res : _int]
                      )))

(define vigra_delete_splineimageview3_c
    (get-ffi-obj 'vigra_delete_splineimageview3_c  vigracket-dylib-path
                 (_fun [siv_ptr : _splineimageview3]
                       -> [res : _int]
                      )))

(define vigra_delete_splineimageview4_c
    (get-ffi-obj 'vigra_delete_splineimageview4_c  vigracket-dylib-path
                 (_fun [siv_ptr : _splineimageview4]
                       -> [res : _int]
                      )))

(define vigra_delete_splineimageview5_c
    (get-ffi-obj 'vigra_delete_splineimageview5_c  vigracket-dylib-path
                 (_fun [siv_ptr : _splineimageview5]
                       -> [res : _int]
                      )))

;;Value
(define vigra_splineimageview1_accessor_c
    (get-ffi-obj 'vigra_splineimageview1_accessor_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_accessor_c
    (get-ffi-obj 'vigra_splineimageview2_accessor_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_accessor_c
    (get-ffi-obj 'vigra_splineimageview3_accessor_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_accessor_c
    (get-ffi-obj 'vigra_splineimageview4_accessor_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_accessor_c
    (get-ffi-obj 'vigra_splineimageview5_accessor_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))

(define vigra_splineimageview1_dx_c
    (get-ffi-obj 'vigra_splineimageview1_dx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dx_c
    (get-ffi-obj 'vigra_splineimageview2_dx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dx_c
    (get-ffi-obj 'vigra_splineimageview3_dx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dx_c
    (get-ffi-obj 'vigra_splineimageview4_dx_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dx_c
    (get-ffi-obj 'vigra_splineimageview5_dx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_dy_c
    (get-ffi-obj 'vigra_splineimageview1_dy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dy_c
    (get-ffi-obj 'vigra_splineimageview2_dy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dy_c
    (get-ffi-obj 'vigra_splineimageview3_dy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dy_c
    (get-ffi-obj 'vigra_splineimageview4_dy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dy_c
    (get-ffi-obj 'vigra_splineimageview5_dy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_dxx_c
    (get-ffi-obj 'vigra_splineimageview1_dxx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dxx_c
    (get-ffi-obj 'vigra_splineimageview2_dxx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dxx_c
    (get-ffi-obj 'vigra_splineimageview3_dxx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dxx_c
    (get-ffi-obj 'vigra_splineimageview4_dxx_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dxx_c
    (get-ffi-obj 'vigra_splineimageview5_dxx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))

(define vigra_splineimageview1_dxy_c
    (get-ffi-obj 'vigra_splineimageview1_dxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dxy_c
    (get-ffi-obj 'vigra_splineimageview2_dxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dxy_c
    (get-ffi-obj 'vigra_splineimageview3_dxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dxy_c
    (get-ffi-obj 'vigra_splineimageview4_dxy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dxy_c
    (get-ffi-obj 'vigra_splineimageview5_dxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))

(define vigra_splineimageview1_dyy_c
    (get-ffi-obj 'vigra_splineimageview1_dyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dyy_c
    (get-ffi-obj 'vigra_splineimageview2_dyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dyy_c
    (get-ffi-obj 'vigra_splineimageview3_dyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dyy_c
    (get-ffi-obj 'vigra_splineimageview4_dyy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dyy_c
    (get-ffi-obj 'vigra_splineimageview5_dyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_dx3_c
    (get-ffi-obj 'vigra_splineimageview1_dx3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dx3_c
    (get-ffi-obj 'vigra_splineimageview2_dx3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dx3_c
    (get-ffi-obj 'vigra_splineimageview3_dx3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dx3_c
    (get-ffi-obj 'vigra_splineimageview4_dx3_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dx3_c
    (get-ffi-obj 'vigra_splineimageview5_dx3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_dxxy_c
    (get-ffi-obj 'vigra_splineimageview1_dxxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dxxy_c
    (get-ffi-obj 'vigra_splineimageview2_dxxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dxxy_c
    (get-ffi-obj 'vigra_splineimageview3_dxxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dxxy_c
    (get-ffi-obj 'vigra_splineimageview4_dxxy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dxxy_c
    (get-ffi-obj 'vigra_splineimageview5_dxxy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_dxyy_c
    (get-ffi-obj 'vigra_splineimageview1_dxyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dxyy_c
    (get-ffi-obj 'vigra_splineimageview2_dxyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dxyy_c
    (get-ffi-obj 'vigra_splineimageview3_dxyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dxyy_c
    (get-ffi-obj 'vigra_splineimageview4_dxyy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dxyy_c
    (get-ffi-obj 'vigra_splineimageview5_dxyy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_dy3_c
    (get-ffi-obj 'vigra_splineimageview1_dy3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_dy3_c
    (get-ffi-obj 'vigra_splineimageview2_dy3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_dy3_c
    (get-ffi-obj 'vigra_splineimageview3_dy3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_dy3_c
    (get-ffi-obj 'vigra_splineimageview4_dy3_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_dy3_c
    (get-ffi-obj 'vigra_splineimageview5_dy3_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_g2_c
    (get-ffi-obj 'vigra_splineimageview1_g2_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_g2_c
    (get-ffi-obj 'vigra_splineimageview2_g2_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_g2_c
    (get-ffi-obj 'vigra_splineimageview3_g2_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_g2_c
    (get-ffi-obj 'vigra_splineimageview4_g2_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_g2_c
    (get-ffi-obj 'vigra_splineimageview5_g2_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_g2x_c
    (get-ffi-obj 'vigra_splineimageview1_g2x_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_g2x_c
    (get-ffi-obj 'vigra_splineimageview2_g2x_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_g2x_c
    (get-ffi-obj 'vigra_splineimageview3_g2x_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_g2x_c
    (get-ffi-obj 'vigra_splineimageview4_g2x_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_g2x_c
    (get-ffi-obj 'vigra_splineimageview5_g2x_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_g2y_c
    (get-ffi-obj 'vigra_splineimageview1_g2y_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_g2y_c
    (get-ffi-obj 'vigra_splineimageview2_g2y_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_g2y_c
    (get-ffi-obj 'vigra_splineimageview3_g2y_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_g2y_c
    (get-ffi-obj 'vigra_splineimageview4_g2y_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_g2y_c
    (get-ffi-obj 'vigra_splineimageview5_g2y_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_g2xx_c
    (get-ffi-obj 'vigra_splineimageview1_g2xx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_g2xx_c
    (get-ffi-obj 'vigra_splineimageview2_g2xx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_g2xx_c
    (get-ffi-obj 'vigra_splineimageview3_g2xx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_g2xx_c
    (get-ffi-obj 'vigra_splineimageview4_g2xx_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_g2xx_c
    (get-ffi-obj 'vigra_splineimageview5_g2xx_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_g2xy_c
    (get-ffi-obj 'vigra_splineimageview1_g2xy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_g2xy_c
    (get-ffi-obj 'vigra_splineimageview2_g2xy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_g2xy_c
    (get-ffi-obj 'vigra_splineimageview3_g2xy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_g2xy_c
    (get-ffi-obj 'vigra_splineimageview4_g2xy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_g2xy_c
    (get-ffi-obj 'vigra_splineimageview5_g2xy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview1_g2yy_c
    (get-ffi-obj 'vigra_splineimageview1_g2yy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview1]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview2_g2yy_c
    (get-ffi-obj 'vigra_splineimageview2_g2yy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview2]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview3_g2yy_c
    (get-ffi-obj 'vigra_splineimageview3_g2yy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) :: [siv_ptr : _splineimageview3]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview4_g2yy_c
    (get-ffi-obj 'vigra_splineimageview4_g2yy_c  vigracket-dylib-path
                 (_fun(siv_ptr x y) :: [siv_ptr : _splineimageview4]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))
(define vigra_splineimageview5_g2yy_c
    (get-ffi-obj 'vigra_splineimageview5_g2yy_c  vigracket-dylib-path
                 (_fun (siv_ptr x y) ::[siv_ptr : _splineimageview5]
                       [x : _double]
                       [y : _double]
                       -> [res : _float]
                      )))

(define (create-splineimageview-band band spline-order)
  (let* ((width  (band-width  band))
	 (height (band-height band)))
    (case spline-order
      ((1) (vigra_create_splineimageview1_c (band-data band)  width height))
      ((2) (vigra_create_splineimageview2_c (band-data band)  width height))
      ((3) (vigra_create_splineimageview3_c (band-data band)  width height))
      ((4) (vigra_create_splineimageview4_c (band-data band)  width height))
      ((5) (vigra_create_splineimageview5_c (band-data band)  width height))
      (else (error "Error in vigracket.splineimageview.create-splineimageview: Splinorder mode must be in {1,2,3,4,5}!!")))))

(define (create-splineimageview image spline-order)
  (map (lambda (band) (create-splineimageview-band band spline-order)) image))

(define create-siv-band create-splineimageview-band)
(define create-siv create-splineimageview)

(define (delete-splineimageview-band siv)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")       (begin  (vigra_delete_splineimageview1_c siv)(set-cpointer-tag! siv  "invalid")))
          ((equal?  siv-type "splineimageview2")       (begin  (vigra_delete_splineimageview2_c siv)(set-cpointer-tag! siv  "invalid")))
          ((equal?  siv-type "splineimageview3")       (begin  (vigra_delete_splineimageview3_c siv)(set-cpointer-tag! siv  "invalid")))
          ((equal?  siv-type "splineimageview4")       (begin  (vigra_delete_splineimageview4_c siv)(set-cpointer-tag! siv  "invalid")))
          ((equal?  siv-type "splineimageview5")       (begin  (vigra_delete_splineimageview5_c siv)(set-cpointer-tag! siv  "invalid")))
          (else (error "Error in vigracket.splineimageview.delete-splineimageview: incompatible pointers used!")))))

(define (delete-splineimageview siv-image)
  (map (lambda (siv) (delete-splineimageview-band siv)) siv-image))

(define delete-siv-band delete-splineimageview-band)
(define delete-siv delete-splineimageview)

;; Subpixel Value
(define (splineimageview-value-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_accessor_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_accessor_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_accessor_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_accessor_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_accessor_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-value: incompatible pointers used!")))))

(define (splineimageview-value siv-image x y)
  (map (lambda (siv) (splineimageview-value-band siv x y)) siv-image))

(define  siv-value-band splineimageview-value-band)
(define  siv-value splineimageview-value)

;;SubPixel dx
(define (splineimageview-dx-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dx_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dx_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dx_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dx_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dx_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dx: incompatible pointers used!")))))

(define (splineimageview-dx siv-image x y)
  (map (lambda (siv) (splineimageview-dx-band siv x y)) siv-image))

(define  siv-dx-band splineimageview-dx-band)
(define  siv-dx splineimageview-dx)

;;SubPixel dy
(define (splineimageview-dy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dy: incompatible pointers used!")))))

(define (splineimageview-dy siv-image x y)
  (map (lambda (siv) (splineimageview-dy-band siv x y)) siv-image))

(define  siv-dy-band splineimageview-dy-band)
(define  siv-dy splineimageview-dy)

;;SubPixel dxx
(define (splineimageview-dxx-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dxx_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dxx_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dxx_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dxx_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dxx_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dxx: incompatible pointers used!")))))

(define (splineimageview-dxx siv-image x y)
  (map (lambda (siv) (splineimageview-dxx-band siv x y)) siv-image))

(define  siv-dxx-band splineimageview-dxx-band)
(define  siv-dxx splineimageview-dxx)

;;SubPixel dxy
(define (splineimageview-dxy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dxy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dxy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dxy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dxy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dxy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dxy: incompatible pointers used!")))))

(define (splineimageview-dxy siv-image x y)
  (map (lambda (siv) (splineimageview-dxy-band siv x y)) siv-image))

(define  siv-dxy-band splineimageview-dxy-band)
(define  siv-dxy splineimageview-dxy)

;;SubPixel dyy
(define (splineimageview-dyy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dyy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dyy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dyy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dyy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dyy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dyy: incompatible pointers used!")))))

(define (splineimageview-dyy siv-image x y)
  (map (lambda (siv) (splineimageview-dyy-band siv x y)) siv-image))

(define  siv-dyy-band splineimageview-dyy-band)
(define  siv-dyy splineimageview-dyy)

;;SubPixel dx3
(define (splineimageview-dx3-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dx3_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dx3_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dx3_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dx3_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dx3_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dx3: incompatible pointers used!")))))

(define (splineimageview-dx3 siv-image x y)
  (map (lambda (siv) (splineimageview-dx3-band siv x y)) siv-image))

(define  siv-dx3-band splineimageview-dx3-band)
(define  siv-dx3 splineimageview-dx3)

;;SubPixel dxxy
(define (splineimageview-dxxy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dxxy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dxxy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dxxy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dxxy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dxxy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dxxy: incompatible pointers used!")))))

(define (splineimageview-dxxy siv-image x y)
  (map (lambda (siv) (splineimageview-dxxy-band siv x y)) siv-image))

(define  siv-dxxy-band splineimageview-dxxy-band)
(define  siv-dxxy splineimageview-dxxy)

;;SubPixel dxyy
(define (splineimageview-dxyy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dxyy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dxyy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dxyy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dxyy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dxyy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dxyy: incompatible pointers used!")))))

(define (splineimageview-dxyy siv-image x y)
  (map (lambda (siv) (splineimageview-dxyy-band siv x y)) siv-image))

(define  siv-dxyy-band splineimageview-dxyy-band)
(define  siv-dxyy splineimageview-dxyy)

;;SubPixel dy3
(define (splineimageview-dy3-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_dy3_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_dy3_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_dy3_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_dy3_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_dy3_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-dy3: incompatible pointers used!")))))

(define (splineimageview-dy3 siv-image x y)
  (map (lambda (siv) (splineimageview-dy3-band siv x y)) siv-image))

(define  siv-dy3-band splineimageview-dy3-band)
(define  siv-dy3 splineimageview-dy3)

;;SubPixel g2
(define (splineimageview-g2-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_g2_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_g2_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_g2_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_g2_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_g2_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-g2: incompatible pointers used!")))))

(define (splineimageview-g2 siv-image x y)
  (map (lambda (siv) (splineimageview-g2-band siv x y)) siv-image))

(define  siv-g2-band splineimageview-g2-band)
(define  siv-g2 splineimageview-g2)


;;SubPixel g2x
(define (splineimageview-g2x-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_g2x_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_g2x_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_g2x_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_g2x_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_g2x_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-g2x: incompatible pointers used!")))))

(define (splineimageview-g2x siv-image x y)
  (map (lambda (siv) (splineimageview-g2x-band siv x y)) siv-image))

(define  siv-g2x-band splineimageview-g2x-band)
(define  siv-g2x splineimageview-g2x)

;;SubPixel g2y
(define (splineimageview-g2y-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_g2y_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_g2y_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_g2y_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_g2y_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_g2y_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-g2y: incompatible pointers used!")))))

(define (splineimageview-g2y siv-image x y)
  (map (lambda (siv) (splineimageview-g2y-band siv x y)) siv-image))

(define  siv-g2y-band splineimageview-g2y-band)
(define  siv-g2y splineimageview-g2y)

;;SubPixel g2xx
(define (splineimageview-g2xx-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_g2xx_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_g2xx_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_g2xx_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_g2xx_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_g2xx_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-g2xx: incompatible pointers used!")))))

(define (splineimageview-g2xx siv-image x y)
  (map (lambda (siv) (splineimageview-g2xx-band siv x y)) siv-image))

(define  siv-g2xx-band splineimageview-g2xx-band)
(define  siv-g2xx splineimageview-g2xx)

;;SubPixel g2xy
(define (splineimageview-g2xy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_g2xy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_g2xy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_g2xy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_g2xy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_g2xy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-g2xy: incompatible pointers used!")))))

(define (splineimageview-g2xy siv-image x y)
  (map (lambda (siv) (splineimageview-g2xy-band siv x y)) siv-image))

(define  siv-g2xy-band splineimageview-g2xy-band)
(define  siv-g2xy splineimageview-g2xy)

;;SubPixel g2yy
(define (splineimageview-g2yy-band siv x y)
  (let ((siv-type  (cpointer-tag siv)))
    (cond ((equal?  siv-type "splineimageview1")        (vigra_splineimageview1_g2yy_c siv x y))
          ((equal?  siv-type "splineimageview2")        (vigra_splineimageview2_g2yy_c siv x y))
          ((equal?  siv-type "splineimageview3")        (vigra_splineimageview3_g2yy_c siv x y))
          ((equal?  siv-type "splineimageview4")        (vigra_splineimageview4_g2yy_c siv x y))
          ((equal?  siv-type "splineimageview5")        (vigra_splineimageview5_g2yy_c siv x y))
          (else (error "Error in vigracket.splineimageview.splineimageview-g2yy: incompatible pointers used!")))))

(define (splineimageview-g2yy siv-image x y)
  (map (lambda (siv) (splineimageview-g2yy-band siv x y)) siv-image))

(define  siv-g2yy-band splineimageview-g2yy-band)
(define  siv-g2yy splineimageview-g2yy)

(provide    create-splineimageview-band
            create-siv-band
            create-splineimageview
            create-siv
            
            delete-splineimageview-band
            delete-siv-band
            delete-splineimageview
            delete-siv
            
            splineimageview-value-band
            siv-value-band
            splineimageview-value 
            siv-value
            
            splineimageview-dx-band
            siv-dx-band
            splineimageview-dx 
            siv-dx
            
            splineimageview-dy-band
            siv-dy-band
            splineimageview-dy 
            siv-dy
            
            splineimageview-dxx-band
            siv-dxx-band
            splineimageview-dxx 
            siv-dxx
            
            splineimageview-dxy-band
            siv-dxy-band
            splineimageview-dxy 
            siv-dxy
            
            splineimageview-dyy-band
            siv-dyy-band
            splineimageview-dyy 
            siv-dyy
            
            splineimageview-dx3-band
            siv-dx3-band
            splineimageview-dx3 
            siv-dx3
            
            splineimageview-dxxy-band
            siv-dxxy-band
            splineimageview-dxxy 
            siv-dxxy
            
            splineimageview-dxyy-band
            siv-dxyy-band
            splineimageview-dxyy 
            siv-dxyy
            
            splineimageview-dy3-band
            siv-dy3-band
            splineimageview-dy3 
            siv-dy3
            
            splineimageview-g2-band
            siv-g2-band
            splineimageview-g2 
            siv-g2
            
            splineimageview-g2x-band
            siv-g2x-band
            splineimageview-g2x 
            siv-g2x
            
            splineimageview-g2y-band
            siv-g2y-band
            splineimageview-g2y 
            siv-g2y
            
            splineimageview-g2xx-band
            siv-g2xx-band
            splineimageview-g2xx 
            siv-g2xx
            
            splineimageview-g2xy-band
            siv-g2xy-band
            splineimageview-g2xy 
            siv-g2xy
            
            splineimageview-g2yy-band
            siv-g2yy-band
            splineimageview-g2yy 
            siv-g2yy)