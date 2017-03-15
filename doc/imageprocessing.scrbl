#lang scribble/manual

@title{Image Processing}

In this chapter, we present ready to use image processing algorithms, which are provided by means
of the VIGRA_C wrapper library as a mediator between the C++ side and the Racket size of the
vigracket module. All of these algorithms are highly optimized and run at high speed.

The end of this chapter form the so called functional extensions, which allow you
to write your own image processing algorithms in an elegant functional stlye. However, the speed
of algorithms written this way, will be a bit below the compiled ones, described before.

Please note that for nearly every function @code{f}, which works on images, there is also a variant
@code{f-band}, which works on single bands only.







@section{Filtering}

This section describe functions, which may be use to filter images to either enhance them or to
derive informations from the image like partial derivatives etc.

@defmodule[vigracket/filters]

@defproc[
 (convolveimage [image image?] [kernel_matrix matrix?])
         image?]{
  Returns the result of the convolution of the image with the given kernel matrix. The kernel matrix should have
  an odd number of rows and columns. Note that this variant is slower than the separable variant for separable
  filter kernels.
}

@defproc[
 (separableconvolveimage [image image?] [kernel_matrix_h matrix?] [kernel_matrix_v matrix?])
         image?]{
  Returns the result of the convolution of the image with given kernel matrices for the horizontal and vertical convolution
  part. The @code{kernel_matrix_h} must have n (odd) rows and exactly one column, whereas the @code{kernel_matrix_v} must
  have exaclty 1 row and n (odd) columns.
}

@defproc[
 (gsmooth [image image?] [scale number?])
         image?]{
  Performs a Gaussian smoothing of the image using a standard deviation of @code{scale}.
}

@defproc[
 (ggradient [image image?] [scale number?])
         image?]{
  Computes the Gaussian gradient magitude of the image using a standard deviation of @code{scale} for the Gaussian first derivative
 convolution kernels.
}

@defproc[
 (gaussiangradient [image image?] [scale number?])
         (list-of image?)]{
  Computes the Gaussian gradient of the image using a standard deviation of @code{scale} for the Gaussian first derivative
 convolution kernels. The result is returned by a list containing two images. The first contains the partial derivatives in x-
 the second contains the partial derivatives in y-direction.
}

@defproc[
 (laplacianofgaussian [image image?] [scale number?])
         image?]{
  Performs an LoG of the image using a standard deviation of @code{scale}.
}

@defproc[
 (hessianmatrixofgaussian [image image?] [scale number?])
         (list-of image?)]{
 Computes the second order Gaussian gradient of the image using a standard deviation of @code{scale} for the Gaussian second derivative
 convolution kernels. The result is returned by a list containing three images. The first contains the partial derivatives in xx-
 the second contains the partial derivatives in xy-, and the third contains the partial derivatives in yy-direction.
}

@defproc[
 (gsharpening [image image?] [sharpening_factor number?] [scale number?])
         image?]{
  Performs a Gaussian sharpening of the image using a standard deviation of @code{scale} and a sharpening factor.
}

@defproc[
 (sharpening [image image?] [scale number?])
         image?]{
  Performs a simple sharpening of the image using a scale of @code{scale} .
}

@defproc[
 (medianfilter [image image?] [window_width number?] [window_height number?])
         image?]{
  Performs a non-linear filtering by means of the median computation of the image using a window of given size.
}

@defproc[
 (nonlineardiffusion [image image?] [edge_threshold number?] [scale number?])
         image?]{
  Performs a non-linear filtering of the image using a scale of @code{scale} to preserve edges above the given threshold.
}

@defproc[
 (shockfilter [image image?] [sigma number?] [rho number?] [upwind_factor_h number?] [iterations number? 1])
         image?]{
  Performs a non-linear filtering of the image using a Ceherence Enhancing Shock Filtering at scale of @code{sigam}.
  For more information on this filter, read the original work at @url{http://www.mia.uni-saarland.de/Publications/weickert-dagm03.pdf}.
}








@section{Tensor-based Processing}

Since the VIGRA library offers a variety of great Tensor-based approaches to play with, some of these are also included in
the VIGRA_C and thus the vigracket module. Here is an overview of the available functions:

@defmodule[vigracket/tensors]

@defproc[
 (structuretensor [image image?] [inner_scale number?] [outer_scale number?])
         (list-of image?)]{
 Computes the Structure Tensor of an image using Gaussian derivation kernels with a standard deviation of @code{inner_scale} and, afterwands,
 Gaussian smoothing kernels with a standard deviation of @code{outer_scale} for the integration part.
 The result is returned by a list containing three images. The first contains the xx-
 the second contains the xy-, and the third contains the yy-part of the tensor's result.
}

@defproc[
 (boundarytensor [image image?] [scale number?])
         (list-of image?)]{
 Computes the Boundary Tensor according to Köthe et al. using a scale of @code{scale}.
 The result is returned by a list containing three images. The first contains the xx-
 the second contains the xy-, and the third contains the yy-part of the tensor's result.
}

@defproc[
 (boundarytensor1 [image image?] [scale number?])
         (list-of image?)]{
 Computes the Boundary Tensor according to Köthe et al. using a scale of @code{scale} but without the DC part.
 The result is returned by a list containing three images. The first contains the xx-
 the second contains the xy-, and the third contains the yy-part of the tensor's result.
}

@defproc[
 (tensoreigenrepresentation [tensor (list-of image?)])
         (list-of image?)]{
 Transforms the three tensor parts to its Eigen representation.
 The result is returned by a list containing three images. The first contains the largest Eigen value
 the second contains the smallest Eigen value, and the third contains the angle of the Eigen values.
}

@defproc[
 (tensortrace [tensor (list-of image?)])
         image?]{
 Computes the trace of a given tensor (mainly: tensor_xx + tensor_yy) and returns this by means of an image.
}

@defproc[
 (tensortoedgecorner [tensor (list-of image?)])
         (list-of image?)]{
 Computes the edgenss (in x- and y-direction) and the cornerness of a given tensor.
 The result is returned by a list containing three images. The first contains the edgeness in x-
 the second contains the edgness in y-direction, and the third contains the cornerness.
}

@defproc[
 (hourglassfilter [tensor (list-of image?)] [sigma number?] [rho number?])
         (list-of image?)]{
 Filters a tensor using Koethe's Houglass-filter approach. Returns the filtered tensor.
}








@section{Image Transformations}

This chapter contains different image transformation algorithms.

@defmodule[vigracket/imgproc]

@defproc[
 (resizeimage [image image?] [new_width exact-number?] [new_height exact-number?] [resize_mode (in-list '(0 1 2 3 4)) 1])
         image?]{
  Resizes the image to new width and height.
  For the inter/extrpolation of the pixel's values, a resize mode may
  be given by the user. 0 means NN-interpolation, 1 means bi-linear interpolation etc.
}

@defproc[
 (rotateimage [image image?] [angle number?] [resize_mode (in-list '(0 1 2 3 4)) 1])
         image?]{
  Rotate the image by means of a given angle.
  Does not change image dimensions!
  For the inter/extrpolation of the pixel's values, a resize mode may
  be given by the user. 0 means NN-interpolation, 1 means bi-linear interpolation etc.
}

@defproc[
 (affinewarpimage [image image?] [affineMatrix matrix?] [resize_mode (in-list '(0 1 2 3 4)) 1])
         image?]{
  Affine tranforamtion of the image by means of a given transformation matrix.
  Does not change image dimensions!
  For the inter/extrpolation of the pixel's values, a resize mode may
  be given by the user. 0 means NN-interpolation, 1 means bi-linear interpolation etc.
}

@defproc[
 (reflectimage [image image?] [reflect_mode (in-list '(1 2 3))])
         image?]{
  Reflects the image either along the horizontal axis (reflect_mode = 1),
  along the vertical axis (resize_mode = 2), or along both axes (resize_mode = 3).
}

@defproc[
 (subimage [image image?] [left number?] [upper number?] [right number?] [lower number?])
         image?]{
  Cuts a rectangular part out of an image and returns it by means of a new image. The upper left pixel is included,
  the lower right pixel is not.
}

@defproc[
 (paddimage [image image?] [left number?] [upper number?] [right number?] [lower number?])
         image?]{
  Padds an image and returns it by means of a new image.
}

@defproc[
 (localminima [image image?] [eight_connectivity boolean? #t])
         image?]{
  Finds the local minima of an image and returns another image, where only these are marked.
  If @code{eight_connectivity} is set to true (default) an eight-neighborhood is used to estimate minima, else
  a four-neighborhood. The search is performed band-wise.
}
@defproc[
 (localmaxima [image image?] [eight_connectivity boolean? #t])
         image?]{
  Finds the local maxima of an image and returns another image, where only these are marked.
  If @code{eight_connectivity} is set to true (default) an eight-neighborhood is used to estimate maxima, else
  a four-neighborhood. The search is performed band-wise.
}








@section{Fourier Transform}

The bindings to the Fast Fourier (and inverse) Transformation need the FFTW lib to be installed
on your system, too!

@defmodule[vigracket/imgproc]

@defproc[
 (fouriertransform [image image?])
         (list-of image?)]{
  Uses the FFTW lib to compute the Fast Fourier Transform of an image. Returns a list of two images,
 where the first contains the real part and the second the imaginary part of the transformation.
}

@defproc[
 (fouriertransforminverse [spectrum (list-of image?)])
         (list-of image?)]{
  Uses the FFTW lib to compute the inverse Fast Fourier Transform of spectrum given its real and imaginary part. Returns a list of two images,
  where the first contains the real part and the second the imaginary part after the inverse transformation.
}








@section{Correlation}

The (cross-) correlation of two images may help finding known pattern in images. For this purpose 
some of the most general and fastest approaches have been made available for use with vigracket.
Please note, that these also need the FFTW lib.

@defmodule[vigracket/imgproc]

@defproc[
 (fastcrosscorrelatíon [image image?] [template image?])
          image?]{
  Uses the Fast Fourier Transform to compute the non-normalized cross correlation between a given image
  and a (usually smaller) template.
}

@defproc[
 (fastnormalizedcrosscorrelation [image image?] [template image?])
         image?]{
  Uses the Fast Fourier Transform to compute the normalized cross correlation correficient between a given image
  and a (usually smaller) template. The values in the result range from -1 to 1.
}








@section{Interpolation}

To make measurements or apply algorithms beyond the pixel boundary, the vigracket module offers a so called
subpixel-view on the image, which lets you access pixels and other measures on the image at non-exact coordinates.
Since this special view is based on a spline interpolation, we refer to it as a SplineImageView:
@defmodule[vigracket/splineimageview]

@defproc[
 (create-splineimageview [image image?] [spline_order (in-list '(1 2 3 4 5))])
         SplineImageView?]{
  Creates a SplineImageView for a given image and spline order.
}

@defproc[
 (delete-splineimageview [siv SplineImageView?])
         void]{
  Destroys a SplineImageView.
}

@defproc[
 (splineimageview-value [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the intensity values at a subpixel position.
}

@defproc[
 (splineimageview-dx [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the first derivative in x-direction at a subpixel position.
}
@defproc[
 (splineimageview-dy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the first derivative in y-direction at a subpixel position.
}

@defproc[
 (splineimageview-dxx [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the second derivative in xx-direction at a subpixel position.
}
@defproc[
 (splineimageview-dxy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the second derivative in xy-direction at a subpixel position.
}
@defproc[
 (splineimageview-dyy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the second derivative in yy-direction at a subpixel position.
}


@defproc[
 (splineimageview-dx3 [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the third derivative in xxx-direction at a subpixel position.
}
@defproc[
 (splineimageview-dxxy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the third derivative in xxy-direction at a subpixel position.
}
@defproc[
 (splineimageview-dxyy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the third derivative in xyy-direction at a subpixel position.
}
@defproc[
 (splineimageview-dy3 [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the third drivative in yyy-direction at a subpixel position.
}


@defproc[
 (splineimageview-g2 [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the squared gradient magnitude (dx*dx + dy*dy) at a subpixel position.
}

@defproc[
 (splineimageview-g2x [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the first derivative of g2 in x-direction at a subpixel position.
}
@defproc[
 (splineimageview-g2y [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the first derivative of g2 in y-direction at a subpixel position.
}

@defproc[
 (splineimageview-g2xx [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the second derivative of g2 in xx-direction at a subpixel position.
}
@defproc[
 (splineimageview-g2xy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the second derivative of g2 in xy-direction at a subpixel position.
}
@defproc[
 (splineimageview-g2yy [siv SplineImageView?] [x number?] [y number?])
         (list-of __float)]{
  Uses a SplineImageView to get the second derivative of g2 in yy-direction at a subpixel position.
}



@section{Morphological Operators}

@defmodule[vigracket/morphology]

@defproc[
 (distancetransform [image image?] [background_label __float] [norm number?])
         image?]{
  Performs a distance transform on an image with a specific background intensity.
}

@section{Segmentation}

@section{Functional Extensions}
