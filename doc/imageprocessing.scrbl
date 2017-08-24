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
 (convolveimage [img image?] [kernel_matrix matrix?] [border_treatment (in-list '(0 1 2 3 4 5))])
         image?]{
  Returns the result of the convolution of the image with the given kernel matrix. The kernel matrix should have
  an odd number of rows and columns. Note that this variant is slower than the separable variant for separable
  filter kernels. The meaning of the border treatment ids is as follows:
  @tabular[#:sep @hspace[1]
           #:style 'boxed
           #:column-properties '(left center left)
           #:row-properties '(bottom-border ())
      (list (list @bold{ID}   @bold{VIGRA's mode} @bold{Description})
            (list "0" "BORDER_TREATMENT_AVOID" "Do not operate on a pixel where the kernel doe not fit in the image}")
            (list "1" "BORDER_TREATMENT_CLIP"  "Clip kernel at image border (this is only useful if the kernel is >= 0 everywhere)")
            (list "2" "BORDER_TREATMENT_REPEAT""Repeat the nearest valid pixel")
            (list "3" "BORDER_TREATMENT_REFLECT" "Reflect image at last row/column")
            (list "4" "BORDER_TREATMENT_WRAP" "Wrap image around (periodic boundary conditions)")
            (list "5" "BORDER_TREATMENT_ZEROPAD" "Assume that all outside points have value zero"))]
  ]
}

@defproc[
 (separableconvolveimage [img image?] [kernel_matrix_h matrix?] [kernel_matrix_v matrix?] [border_treatment (in-list '(0 1 2 3 4 5))])
         image?]{
  Returns the result of the convolution of the image with given kernel matrices for the horizontal and vertical convolution
  part. The @code{kernel_matrix_h} must have n (odd) rows and exactly one column, whereas the @code{kernel_matrix_v} must
  have exaclty 1 row and n (odd) columns. For the meaning of the border treatment ids, look above.
}

@defproc[
 (gsmooth [img image?] [scale number?])
         image?]{
  Performs a Gaussian smoothing of the image using a standard deviation of @code{scale}.
}

@defproc[
 (ggradient [img image?] [scale number?])
         image?]{
  Computes the Gaussian gradient magitude of the image using a standard deviation of @code{scale} for the Gaussian first derivative
 convolution kernels.
}

@defproc[
 (gaussiangradient [img image?] [scale number?])
         (list-of image?)]{
  Computes the Gaussian gradient of the image using a standard deviation of @code{scale} for the Gaussian first derivative
 convolution kernels. The result is returned by a list containing two images. The first contains the partial derivatives in x-
 the second contains the partial derivatives in y-direction.
}

@defproc[
 (laplacianofgaussian [img image?] [scale number?])
         image?]{
  Performs an LoG of the image using a standard deviation of @code{scale}.
}

@defproc[
 (hessianmatrixofgaussian [img image?] [scale number?])
         (list-of image?)]{
 Computes the second order Gaussian gradient of the image using a standard deviation of @code{scale} for the Gaussian second derivative
 convolution kernels. The result is returned by a list containing three images. The first contains the partial derivatives in xx-
 the second contains the partial derivatives in xy-, and the third contains the partial derivatives in yy-direction.
}

@defproc[
 (gsharpening [img image?] [sharpening_factor number?] [scale number?])
         image?]{
  Performs a Gaussian sharpening of the image using a standard deviation of @code{scale} and a sharpening factor.
}

@defproc[
 (sharpening [img image?] [scale number?])
         image?]{
  Performs a simple sharpening of the image using a scale of @code{scale} .
}

@defproc[
 (medianfilter [img image?] [window_width number?] [window_height number?])
         image?]{
  Performs a non-linear filtering by means of the median computation of the image using a window of given size.
}

@defproc[
 (nonlineardiffusion [img image?] [edge_threshold number?] [scale number?])
         image?]{
  Performs a non-linear filtering of the image using a scale of @code{scale} to preserve edges above the given threshold.
}

@defproc[
 (shockfilter [img image?] [sigma number?] [rho number?] [upwind_factor_h number?] [iterations number? 1])
         image?]{
  Performs a non-linear filtering of the image using a Ceherence Enhancing Shock Filtering at scale of @code{sigam}.
  For more information on this filter, read the original work at @url{http://www.mia.uni-saarland.de/Publications/weickert-dagm03.pdf}.
}








@section{Tensor-based Processing}

Since the VIGRA library offers a variety of great Tensor-based approaches to play with, some of these are also included in
the VIGRA_C and thus the vigracket module. Here is an overview of the available functions:

@defmodule[vigracket/tensors]

@defproc[
 (structuretensor [img image?] [inner_scale number?] [outer_scale number?])
         (list-of image?)]{
 Computes the Structure Tensor of an image using Gaussian derivation kernels with a standard deviation of @code{inner_scale} and, afterwands,
 Gaussian smoothing kernels with a standard deviation of @code{outer_scale} for the integration part.
 The result is returned by a list containing three images. The first contains the xx-
 the second contains the xy-, and the third contains the yy-part of the tensor's result.
}

@defproc[
 (boundarytensor [img image?] [scale number?])
         (list-of image?)]{
 Computes the Boundary Tensor according to Köthe et al. using a scale of @code{scale}.
 The result is returned by a list containing three images. The first contains the xx-
 the second contains the xy-, and the third contains the yy-part of the tensor's result.
}

@defproc[
 (boundarytensor1 [img image?] [scale number?])
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
 (resizeimage [img image?] [new_width exact-number?] [new_height exact-number?] [resize_mode (in-list '(0 1 2 3 4)) 1])
         image?]{
  Resizes the image to new width and height.
  For the inter/extrpolation of the pixel's values, a resize mode may
  be given by the user. 0 means NN-interpolation, 1 means bi-linear interpolation etc.
}

@defproc[
 (rotateimage [img image?] [angle number?] [resize_mode (in-list '(0 1 2 3 4)) 1])
         image?]{
  Rotate the image by means of a given angle.
  Does not change image dimensions!
  For the inter/extrpolation of the pixel's values, a resize mode may
  be given by the user. 0 means NN-interpolation, 1 means bi-linear interpolation etc.
}

@defproc[
 (affinewarpimage [img image?] [affineMatrix matrix?] [resize_mode (in-list '(0 1 2 3 4)) 1])
         image?]{
  Affine tranforamtion of the image by means of a given transformation matrix.
  Does not change image dimensions!
  For the inter/extrpolation of the pixel's values, a resize mode may
  be given by the user. 0 means NN-interpolation, 1 means bi-linear interpolation etc.
}

@defproc[
 (reflectimage [img image?] [reflect_mode (in-list '(1 2 3))])
         image?]{
  Reflects the image either along the horizontal axis (reflect_mode = 1),
  along the vertical axis (resize_mode = 2), or along both axes (resize_mode = 3).
}

@defproc[
 (subimage [img image?] [left number?] [upper number?] [right number?] [lower number?])
         image?]{
  Cuts a rectangular part out of an image and returns it by means of a new image. The upper left pixel is included,
  the lower right pixel is not.
}

@defproc[
 (paddimage [img image?] [left number?] [upper number?] [right number?] [lower number?] [value (list-of _float?) '()])
         image?]{
  Padds an image by the given color intensity list value and returns it by means of a new image. If no intensity list for value is given, the border will be filled with zeros/black.
}

@defproc[
 (localminima [img image?] [eight_connectivity boolean? #t])
         image?]{
  Finds the local minima of an image and returns another image, where only these are marked.
  If @code{eight_connectivity} is set to true (default) an eight-neighborhood is used to estimate minima, else
  a four-neighborhood. The search is performed band-wise.
}
@defproc[
 (localmaxima [img image?] [eight_connectivity boolean? #t])
         image?]{
  Finds the local maxima of an image and returns another image, where only these are marked.
  If @code{eight_connectivity} is set to true (default) an eight-neighborhood is used to estimate maxima, else
  a four-neighborhood. The search is performed band-wise.
}

The bindings to the Fast Fourier (and inverse) Transformation need the FFTW lib to be installed
on your system, too!

@defproc[
 (fouriertransform [img image?])
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

The (cross-) correlation of two images may help finding known pattern in images. For this purpose 
some of the most general and fastest approaches have been made available for use with vigracket.
Please note, that these also need the FFTW lib.


@defproc[
 (fastcrosscorrelation [img image?] [template image?])
          image?]{
  Uses the Fast Fourier Transform to compute the non-normalized cross correlation between a given image
  and a (usually smaller) template.
}

@defproc[
 (fastnormalizedcrosscorrelation [img image?] [template image?])
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
 (create-splineimageview [img image?] [spline_order (in-list '(1 2 3 4 5))])
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

Morphological operations are commonly known and defined on binary images. A binary image is an image,
which can be separated into two classes for each pixel: background (value = 0) and foreground. Based on this classification
operations may be performed.

@defmodule[vigracket/morphology]

@defproc[
 (distancetransform [img image?] [background_label __float] [norm (in-list '(0 1 2))])
         image?]{
  Performs a distance transform on an image with a specific background intensity. Afterwards, each pixel contains
  the distance to the background w.r.t. the given distance norm (0 = checker board, 1 = Manhattan, 2  = Euclidean distance).
}

@defproc[
 (erodeimage [img image?] [radius exact-number?])
         image?]{
  Performs a morphological erosion operation on a binary image using a given erosion radius.
}

@defproc[
 (dilateimage [img image?] [radius exact-number?])
         image?]{
  Performs a morphological dilation operation on a binary image using a given dilation radius.
}


@defproc[
 (openingimage [img image?] [radius exact-number?])
         image?]{
  Performs a morphological opening operation (erosion then dilation) on a binary image using a given radius.
}


@defproc[
 (closingimage [img image?] [radius exact-number?])
         image?]{
  Performs a morphological closing operation (dilation then erosion) on a binary image using a given radius.
}








@section{Segmentation}

The task of segmentation is to dived the image plane into groups of somehow similar pixel groups, the so-called 
segments. The easiest form of such a sgmentation may be performed by a thresholding operation. However, after thesholding
the image, the computer does not know about the connected components (or the segments) since each pixel is either classified as
foreground or background. Here, the union-find labelling can be used to get a lable/id based segmentation:

@defmodule[vigracket/segmentation]

@defproc[
 (labelimage [img image?] [eight_connectivity boolean? #t])
         image?]{
  Performs a union-find labelling to assign unique labels to a e.g. binarized image. If eight_connectivity is set to true (default),
  the union-find algorithm will use the eight-neighborhood for component labelling.
}

@defproc[
 (watersheds-uf [img image?] [eight_connectivity boolean? #t])
         image?]{
  Performs a union-find watershed segmentation approach, where each image minimum will correspont to one segment.
  To get meaningful results, apply this on the result of @code{(ggradient image)} instead of the image.
  If eight_connectivity is set to true (default),
  the union-find algorithm will use the eight-neighborhood for component labelling.
}

@defproc[
 (watersheds-rg [img image?] [eight_connectivity boolean? #t])
         image?]{
  Performs a region-growing watershed segmentation approach, where each image minimum will correspont to one segment.
  To get meaningful results, apply this on the result of @code{(ggradient image)} instead of the image.
  If eight_connectivity is set to true (default),
  the region-growing algorithm will use the eight-neighborhood for component labelling.
}

@defproc[
 (slic [img image?] [seedDistance number? 15] [intensityScaling number? 20.0] [iterations exact-number? 40])
         image?]{
  Performs a SLIC segmentation on the given image. The seedDistance controls the inital seed distribution on the image.
  The intensityScaling determined the rule of the image intensity over the geometrical distance. Eventually, the number
  of iterations may be adapted. This function does not work bandwise for 3-band images, but uses the LUV-conversion in this case.
}

Given an image and its segmentation  by means of a label image, we might be interested in some of the regions' statistics.
To get these, vigracket offers basic access to some of the most important region features using fast image and label traversal:


@defproc[
 (extractfeatures [img image?] [labels image?] [max_label (image-reduce max labels 0)])
         image?]{
  Determines the features for an image and a label image. The y-coordinate of the resulting image determines the region id,
  for which the features shall be queried. The features are computed in a band-wise manner if the number of bands for image and labels
  are equal. In this case, the resulting image is of size (17 x max_label x numBands) and has the following semantics:
 @tabular[#:sep @hspace[1]
          (list (list @bold{Index} @bold{Feature})
                (list "0" 	"region_size")
                (list "1, 2" 	"upperleft-x and y-coord")
                (list "3, 4"	"lowerright-x and y-coord")
                (list "5, 6" 	"mean-x and y-coord")
                (list "7"	"min grey value")
                (list "8" 	"max grey value")
                (list "9" 	"mean grey value")
                (list "10" 	"std.dev. grey value")
                (list "11, 12" 	"major eigenvector: x and y-coord")
                (list "13, 14" 	"minor eigenvector: x and y-coord")
                (list "15"      "major eigenvalue")
                (list "16"      "minor eigenvalue"))]

  If the image has three bands, and the labels have one band, RGB-features will be extracted instead. Here,
  the resulting image is of size (25 x max_label x numBands) and has the following semantics:
 @tabular[#:sep @hspace[1]
          (list (list @bold{Index} @bold{Feature})
                (list "0" 	"region_size")
                (list "1, 2" 	"upperleft-x and y-coord")
                (list "3, 4" 	"lowerright-x and y-coord")
                (list "5, 6" 	"mean-x and y-coord")
                (list "7, 8, 9" 	"min red,green,blue value")
                (list "10, 11, 12" 	"max red,green,blue value")
                (list "13, 14, 15" 	"mean red,green,blue value")
                (list "16, 17, 18"	"std.dev. red,green,blue value")
                (list "19, 20" 	"major eigenvector: x and y-coord")
                (list "21, 22" 	"minor eigenvector: x and y-coord")
                (list "23"      "major eigenvalue")
                (list "24"      "minor eigenvalue"))]
}

For the visualization of a label image, a crack edge image may be appropriate. This image can e.g. be generated from a label image
and allows the explicit drawing of edges between the segments due to its increased size of (2 x width - 1, 2 x height -1):

@defproc[
 (regionimagetocrackedgeimage [img image?] [marker number? 0.0])
         image?]{
  Creates a crack edge image representation of a segmentation and marks edges between the segments with the given intensity marker.
}

Another view on segmentation is use edge dectors. Here, the vigracket module offers two famous edge detectors:

@defproc[
 (cannyedgeimage [img image?] [scale number?] [gradient_threshold number?] [mark number?])
         image?]{
  Uses the Canny approach to find edges at a given scale, which are above a given gradient threshold. Returns an image, where all the found
  edgels are marked with the given marker value, else 0.
}

@defproc[
 (differenceofexponentialedgeimage [img image?] [scale number?] [gradient_threshold number?] [mark number?])
         image?]{
  Uses the difference of exponential edge approach to find edges at a given scale, which are above a given gradient threshold. Returns an image, where all the found
  edgels are marked with the given marker value, else 0.
}