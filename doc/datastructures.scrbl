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
 (image-width [image image?])
         integer?]{
  Returns the width of the first band of the image.
}

@defproc[
 (image-height [image image?])
         integer?]{
  Returns the height of the first band of the image.
}
@defproc[
 (image-numbands [image image?])
         integer?]{
  Returns the number of bands of the image. Same as @code{(length image)}.
}

@defproc[
 (image-band [image image?] [band_id integer?])
         carray?]{
  Returns a single band of an imageby means of the two dimensional @code{carray}. If the band_id is larger than the number
  of bands oth the image, an error is thrown. Same as @code{(list-ref image band_id)}.
}

@defproc[
 (image-data [image image?] [band_id integer?])
         cvector?]{
  Returns a single band of an image by means of a one dimensional (flat) foreign memory array. Width and height information is lost here.
}


@defproc[
 (image-ref [image image?]
            [x (and (integer? x) (< -1 x (image-width image)))]
            [y (and (integer? y) (< -1 y (image-height image)))]
            [band_id integer? '()])
         (or __float (list-of __float))]{
  If called with a band_id argument it return the single image intensity at the given position and the image band. If called without band_id,
  it returns a list of all the pixel's values ordered in the same way as the basnd of the image.
}

@defproc[
 (image-set! [image image?]
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
 (copy-image [image image?])
        image?]{
  Returns a copy of the given image with freshly allocated foreing memory.
}

@defproc[
 (image->red-band [image image?])
         cvector?]{
  Returns the first band of the image, which is considered to contain the red intensities.
}
@defproc[
 (image->green-band [image image?])
         cvector?]{
  Returns the second band of the image, which is considered to contain the green intensities.
}
@defproc[
 (image->blue-band [image image?])
         cvector?]{
  Returns the third band of the image, which is considered to contain the blue intensities.
}
@defproc[
 (image->alpha-band [image image?])
         cvector?]{
  Returns the fourth band of the image, which is considered to contain the alpha/transparency intensities.
}


@defproc[
 (image->red [image image?])
         image?]{
  Returns a new image containing only the first band of the image, which is considered to contain the red intensities.
}
@defproc[
 (image->green [image image?])
         image?]{
 Returns a new image containing only the second band of the image, which is considered to contain the green intensities.
}
@defproc[
 (image->blue [image image?])
         image?]{
  Returns a new image containing only the third band of the image, which is considered to contain the blue intensities.
}
@defproc[
 (image->alpha [image image?])
         image?]{
  Returns a new image containing only the fourth band of the image, which is considered to contain the alpha/transparency intensities.
}

@defproc[
 (image->list [image image?])
         (list-of (list-of list?))]{
  Returns a list of lists of lists containing all pixel intensities in Racket-managed memory.
  @bold{Please avoid this function for images, since is slows down algorithms drastically.
  Use the high-order functional programming interface instead!}
}

@defproc[
 (list->image [list (list-of (list-of list?))])
         image?]{
  Given a list of lists of lists containing all pixel intensities in Racket-managed memory this allocated a new image and fills it with the
  intensity values provided.
  @bold{Please avoid this function for images, since is slows down algorithms drastically.
  Use the high-order functional programming interface instead!}
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
 (matrix-rows [matrix matrix?])
         integer?]{
  Returns the row count of the matrix.
}

@defproc[
 (matrix-cols [matrix matrix?])
         integer?]{
  Returns the column count of the matrix.
}

@defproc[
 (matrix-data [matrix matrix?])
         cvector?]{
  Returns the matrix' data by means of a one dimensional (flat) foreign memory array. Row and column count information is lost here.
}


@defproc[
 (matrix-ref [matrix matrix?]
            [row (and (integer? row) (< -1 row (matrix-rows matrix)))]
            [col (and (integer? col) (< -1 col (matrix-cols matrix)))])
         __double]{
  Return the matrix value at the given row and column.
}

@defproc[
 (matrix-set! [matrix matrix?]
            [row (and (integer? row) (< -1 row (matrix-rows matrix)))]
            [col (and (integer? col) (< -1 col (matrix-cols matrix)))]
            [value __double])
         void]{
  Replaces he matrix value at the given row and column with a new value.
  Note, that this is a non-functional approach of data manipulation!
}

@defproc[
 (copy-matrix [matrix matrix?])
        matrix?]{
  Returns a copy of the given matrix with freshly allocated foreing memory.
}

@defproc[
 (matrix->list [matrix matrix?])
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