#lang scribble/manual

@title{Datastructures}
@defmodule[vigracket/helpers]

Apart from Rackets data structures and data types, the vigracket adds another
data container for arbitrary dimensional arrays of foreign memory @code{carray}. This is realized
by mapping the (probably) n dimension onto a flat @code{cvector} of the corresponding C data
type.

Although generally multi-dimensional, we only use the two dimensional specialization of the @code{carray}
to implement all of the data structures described below. To keep the memory use as low as possible, we
do not store a non-foreign copy of the data structures.

@section{CArrays}
@defmodule[vigracket/carray]

The common use of the @code{vigra_c} wrapper-library is as follows: Allocate memory on client-side (here: Racket),
execute the function on the server side (C/C++), and then return the previously allocated datastructures by 
means of an images. Thus, we need an array-like datastructure, which is allocated by Racket by accessible in
so-called foreign-memory space (more-or-less global valid pointer).

For flat arrays, the Racket type @code{cvector} fulfilles these requirements, since it combines a pointer,
and data type and a length of the array with construction and destruction in foreign memory.
However, since we are dealing with images, we need a proper 2D interpretation of this flat field.
Here, the @code{carray} data type comes into play, enriching the @code{cvector} data type with an n-dimensional
shape and n-dimensional access operations.

@defproc[
 (make-carray [type ctype?]
              [dimensions (list-of integer?)])
         carray?]{
  Returns a carray of a given ffi c-type and a given size (dimensions). The carray will be allocated with an overall size of
  @code{(apply * dimensions)}.
  
  Example: to allocate a 2D-array of type @code{_float} and a size of @code{100x200} write: 
  @code{(define carr (make-carray _float '(100 200)))}.
}

@defproc[
 (carray-type [carray carray?])
       ctype?]{
 Returns the @code{ctype} of the given carray. For image-bands, this is always @code{_float}, for matrices: @code{_double}.
}
 
@defproc[
 (carray-dimensions [carray carray?])
       (list-of integer?)]{
 Returns the n-dimensional shape of this @code{carray} by means of a list with n integers.
}
  
@defproc[
 (carray-index [pos (list-of integer?)]
               [dimensions (list-of integer?)])
        integer?]{
 Returns the flat line-scan index for the n-dimensional position and shape of a @code{carray} as a single integer.
 Example:
 
 @code{(carray-index '(1 1 1) '(10 20 30)) = 1 + 1*10 + 1*(10*20) = 211}

 This function is used to allow access to the underlying flat data of an @code{carray}.
}

@defproc[
 (carray-ref [arr carray?]
             [pos (list-of integer?)])
         ctype?]{
  Returns the value of the @code{carray} at the n-dimensional position @code{pos}. Will throw an error, if the given
  position is out of range of the shape of the given array. Might be slow due to the checking, but safe!
}

@defproc[
 (carray-ref/unsafe [arr carray?]
                    [pos (list-of integer?)])
         ctype?]{
 Returns the value of the @code{carray} at the n-dimensional position @code{pos}, but does not check if the index is inside
 the bounds of the array's dimensions. If an invalid index is given, the behaviour will be undefined - from reading different
 memory content up to a crash.
}

@defproc[
 (carray-set! [arr carray?]
              [pos (list-of integer?)]
              [val ctype?])
         void?]{
  Sets the value of the @code{carray} at the n-dimensional position @code{pos} to @code{val}. Will throw an error, if the given
  position is out of range of the shape of the given array. Might be slow due to the checking, but safe!
}

@defproc[
 (carray-set/unsafe! [arr carray?]
                     [pos (list-of integer?)]
                     [val ctype?])
         void?]{
 Sets the value of the @code{carray} at the n-dimensional position @code{pos} to @code{val}, but does not check if the index is inside
 the bounds of the array's dimensions. If an invalid index is given, the behaviour will be undefined - from setting different
 memory content up to a crash.
}

@defproc[
 (copy-carray [arr carray?])
        carray?]{
  Returns a copy of the given @code{carray} with freshly allocated foreign memory.
}


@defproc[
 (carray-map [func procedure?] [arr carray?] ... )
         carray?]{
  Works like the map-function for lists, assuming that each carray can be considered as a list of value lists. The count of carrays to pass
  to this function depends on the number of arguments of the function. E.g. @code{(carray-map + arr1 arr2 arr3)} adds three carrays together.
  This function always allocates new foreign memory and does not manipulate the input carrays.
}

@defproc[
 (carray-map! [func procedure?] [arr carray?] ... )
         carray?]{
  Same as carray-map, but stores the result in the first given carray by overwriting the original carray's contents.
}

@defproc[
 (carray-foldl [func procedure?] [seed any/c?] [arr carray?])
         ctype?]{
  Works like the foldl-function for lists, assuming that each carray can be considered as a list of value lists.
}
@defproc[
 (carray-foldr [func procedure?] [seed any/c?] [arr carray?])
         ctype?]{
  Works like the foldr-function for lists, assuming that each carray can be considered as a list of value lists.
}
@defproc[
 (carray-reduce [func procedure?] [arr carray?] [seed any/c?])
         ctype?]{
  Same as @code{(carray-foldr func seed carray)}. Still there for compatibilty issues.
}


@defproc[
 (carray->list [arr carray?])
         (list-of ... (list-of ctype?))]{
  Returns an n-dimensional list containing all values of the Racket-managed memory.
  @bold{Please avoid this function for carrays, since is slows down algorithms drastically.
  Use the high-order functional programming interface instead!}
}

@defproc[
 (list->carray [list (list-of ... (list-of ctype?))])
         carray?]{
  Given an n-dimensional lists of lists containing all values in Racket-managed memory,
  this allocates a @code{carray} and fills it with the provided values.
  @bold{Please avoid this function for carrays, since is slows down algorithms drastically.
  Use the high-order functional programming interface instead!}
}









@section{Images}

Images are modeled as lists. Each list item is one two dimensional @code{carray} of value type @code{_float}. For most 
application, the ordering of the list is not fixed to some semantics. However, for display purpose and some other 
conversion functions, the following semantics are intended:

@tabular[#:sep @hspace[1]
         (list (list @bold{Number of items} @bold{Intended sematics (order)})
               (list "1"       "grey value (intensity) image")
               (list "3"       "R-, G-, B-valued color image")
               (list "4"       "R-, G-, B-valued color image with A-lpha channel as fourth item."))]

Please note that a @code{carray} alone is not an image but may be considered as a single band of pixel data
of an image.


@defproc[
 (make-image [width integer?] [height integer?] [numBands integer?] ...)
         image?]{
  Returns an image of given size and band count. The image data will be allocated in foreign memory
  space. If there are more arguments than the three for the dimensions, these have to be as many floats as
 @code{numBands} and will be used to initialize the bands' values.
}

@defproc[
 (image-width [img image?])
         integer?]{
  Returns the width of the first band of the image.
}

@defproc[
 (image-height [img image?])
         integer?]{
  Returns the height of the first band of the image.
}
@defproc[
 (image-numbands [img image?])
         integer?]{
  Returns the number of bands of the image. Same as @code{(length image)}.
}

@defproc[
 (image-band [img image?] [band_id integer?])
         carray?]{
  Returns a single band of an imageby means of the two dimensional @code{carray}. If the band_id is larger than the number
  of bands oth the image, an error is thrown. Same as @code{(list-ref image band_id)}.
}

@defproc[
 (image-data [img image?] [band_id integer?])
         cvector?]{
  Returns a single band of an image by means of a one dimensional (flat) foreign memory array. Width and height information is lost here.
}


@defproc[
 (image-ref [img image?]
            [x (and (integer? x) (< -1 x (image-width image)))]
            [y (and (integer? y) (< -1 y (image-height image)))]
            [band_id integer? '()])
         (or __float (list-of __float))]{
  If called with a band_id argument it return the single image intensity at the given position and the image band. If called without band_id,
  it returns a list of all the pixel's values ordered in the same way as the basnd of the image.
}

@defproc[
 (image-set! [img image?]
            [x (and (integer? x) (< -1 x (image-width image)))]
            [y (and (integer? y) (< -1 y (image-height image)))]
            [band_id-or-value (or integer?  (list-of __float))]
            [value __float '()])
         void]{
  If called with a band_id-or-value and  value argument it replaces the single image intensity at the given position and the image band.
  If called without value, it uses the list given band_id-or-value to set the pixels on all bands accordingly.
  Note, that this is a non-functional approach of data manipulation!
}

@defproc[
 (copy-image [img image?])
        image?]{
  Returns a copy of the given image with freshly allocated foreign memory.
}

@defproc[
 (initimage [img image?] [value __float?] ...)
        image?]{
  Sets all pixels of the given image to the values and returns the initialized image. Does work in-place!
  As an example, to re-initialize a three band RGB-image @code{img} with zeros, you might call @code{(initimage img 0.0 0.0 0.0)}.
}

@defproc[
 (image->red-band [img image?])
         carray?]{
  Returns the first band of the image, which is considered to contain the red intensities.
}
@defproc[
 (image->green-band [img image?])
         carray?]{
  Returns the second band of the image, which is considered to contain the green intensities.
}
@defproc[
 (image->blue-band [img image?])
         carray?]{
  Returns the third band of the image, which is considered to contain the blue intensities.
}
@defproc[
 (image->alpha-band [img image?])
         carray?]{
  Returns the fourth band of the image, which is considered to contain the alpha/transparency intensities.
}


@defproc[
 (image->red [img image?])
         image?]{
  Returns a new image containing only the first band of the image, which is considered to contain the red intensities.
}
@defproc[
 (image->green [img image?])
         image?]{
 Returns a new image containing only the second band of the image, which is considered to contain the green intensities.
}
@defproc[
 (image->blue [img image?])
         image?]{
  Returns a new image containing only the third band of the image, which is considered to contain the blue intensities.
}
@defproc[
 (image->alpha [img image?])
         image?]{
  Returns a new image containing only the fourth band of the image, which is considered to contain the alpha/transparency intensities.
}

@defproc[
 (image->list [img image?])
         (list-of (list-of list?))]{
  Returns a list of lists of lists containing all pixel intensities in Racket-managed memory.
  @bold{Please avoid this function for images, since is slows down algorithms drastically.
  Use the high-order functional programming interface instead!}
}

@defproc[
 (list->image [list (list-of (list-of list?))])
         image?]{
  Given a list of lists of lists containing all pixel intensities in Racket-managed memory, this allocates a new image and fills it with the
  intensity values provided.
  @bold{Please avoid this function for images, since is slows down algorithms drastically.
  Use the high-order functional programming interface instead!}
}

@defproc[
 (image-map [func procedure?] [img image?] ... )
         image?]{
  Works like the map-function for lists, assuming that each image can be considered as a list of value lists. The count of images to pass
  to this function depends on the number of arguments of the function. E.g. @code{(image-map + img1 img2 img3)} adds three images together.
  This function always allocates new foreign memory and does not manipulate the input images.
  Unlike the basic mapping function, this function establishes band-broadcasting and may thus be used to combine multi- and single band images.
  E.g. it is possbile to call the mapping function with images of 3, 1, 1 bands but not with 3, 2, 1 bands. Single band images will be pumped up to
  the maximum band count.
}

@defproc[
 (image-map! [func procedure?] [img image?] ... )
         image?]{
  Same as image-map, but stores the result in the first given image by overwriting the original image contents.
}

@defproc[
 (image-foldl [func procedure?] [seed any/c?] [img image?])
         list?]{
  Works like the foldl-function for lists, assuming that each image can be considered as a list of value lists. This function works band-wise
  and applies the given function internally on each band of the image.
}
@defproc[
 (image-foldr [func procedure?] [seed any/c?] [img image?])
         list?]{
  Works like the foldr-function for lists, assuming that each image can be considered as a list of value lists. This function works band-wise
  and applies the given function internally on each band of the image.
}
@defproc[
 (image-reduce [func procedure?] [img image?] [seed any/c?])
         list?]{
  Same as @code{(image-foldr func seed image)}. Still there for compatibilty issues.
}

Many algorithms work by means of an outer image traversal. To encapsulate this, the functional extension of vigracket has several supporting functions:

@defproc[
 (image-for-each-index [func procedure?] [img image?])
         void]{
  Calls a given procedure @code{func} with arguments x, y, band_id (e.g. @code{(lambda (x y band_id) (void))}) for every possible index (combination of x,y and band_id)
 of the given image. If the result shall be visible, the function @code{func} must use side-effects, e.g. by means of @code{image-set!}.
}

@defproc[
 (image-for-each-pixel [func procedure?] [img image?])
         void]{
  Calls a given procedure @code{func} with arguments x, y, pixel_value (e.g. @code{(lambda (x y value) (void))}) for every possible pixel (combination of x,y)
 of the given image with the bands intensities listed. If the result shall be visible, the function @code{func} must use side-effects, e.g. by means of @code{image-set!}.
}

@defproc[
 (image-while [pred procedure?] [img image?])
         void]{
  Calls a given procedure @code{pred} with arguments x, y, pixel_value (e.g. @code{(lambda (x y value) #t)}) for every possible pixel (combination of x,y)
 of the given image with the bands intensities listed until the result of pred is false. If the result shall be persistent, the function @code{func} must use side-effects to store it.
}

@section{Matrices}

Some algorithms need matrices, e.g. for the application or detection of geometrical transformation or 
convolutional operators by means of a convolution kernel. In contrast to images, matrices are not modeled as
lists but as a single @code{carray} of value type @code{_double}. Since matrices are ofter smaller, the increased
memory use is still within acceptable boundaries.

@defproc[
 (make-matrix [rows integer?] [cols integer?] [init-val number? 0])
         matric?]{
  Returns a matrix of given size, which will be allocated in foreign memory space.
}

@defproc[
 (matrix-rows [mat matrix?])
         integer?]{
  Returns the row count of the matrix.
}

@defproc[
 (matrix-cols [mat matrix?])
         integer?]{
  Returns the column count of the matrix.
}

@defproc[
 (matrix-data [mat matrix?])
         cvector?]{
  Returns the matrix' data by means of a one dimensional (flat) foreign memory array. Row and column count information is lost here.
}


@defproc[
 (matrix-ref [mat matrix?]
            [row (and (integer? row) (< -1 row (matrix-rows matrix)))]
            [col (and (integer? col) (< -1 col (matrix-cols matrix)))])
         __double]{
  Return the matrix value at the given row and column.
}

@defproc[
 (matrix-set! [mat matrix?]
            [row (and (integer? row) (< -1 row (matrix-rows matrix)))]
            [col (and (integer? col) (< -1 col (matrix-cols matrix)))]
            [value __double])
         void]{
  Replaces he matrix value at the given row and column with a new value.
  Note, that this is a non-functional approach of data manipulation!
}

@defproc[
 (copy-matrix [mat matrix?])
        matrix?]{
  Returns a copy of the given matrix with freshly allocated foreign memory.
}

@defproc[
 (matrix->list [mat matrix?])
         (list-of (list-of number?))]{
  Returns a list of lists of values containing all matrix values in Racket-managed memory.
}

@defproc[
 (list->matrix [list (list-of (list-of number?))])
         matrix?]{
  Given a list of lists of values containing all matrix values in Racket-managed memory this allocated a new matrix and fills it with the
  values provided.
}

@defproc[
 (matrix-mult [mat1 matrix?] [mat2 matrix?])
         matrix?]{
  Uses the matrix multiplication to multiply both matrices.
}