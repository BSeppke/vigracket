#lang scribble/manual

@title{Examples}

In this section, we have collected some very basic examples. More examples and uses of the
included image processing algorithms can be found in the file "examples.rkt", which is part
of the vigracket module.

@section{Hello Image}

Instead of a classical "Hello World", we present a "Hello Image" program. This program demonstrates
the use of the vigracket module to load, show and save images. Before we start, we need to load the
vigracket itself:

@racketblock[(require vigracket)]

Now we can load an arbitrary image, e.g. the Blocks-World image, which is part of the vigracket module:

@racketblock[
 (define myImage
         (load-image (build-path vigracket-path "images/blox.gif")))]

Afterwards, the symbol @code{myImage} contains the image in the vigracket representation (a list of intensity arrays).
Since the image is greyscale, we will have a list with one element, the array of grey values.
We now may want to take a closer look to the image using the included image viewer:

@racketblock[(show-image myImage "Blox image")]

This opens the viewer in a new window and shows the image.

If we are interested in a few statistics of the image, we may use the functional extensions. As an example, to
estimate the maximum intensity value of the loaded @code{myImage}, we can use the folding higher order function @code{image-foldl} like:

@racketblock[(define image-max (image-foldl max 0.0 myImage))]

If we want to save the image without any procedures, we can enter:

@racketblock[(save-image myImage (build-path vigracket-path "images/blox.png"))]

This saves the image in the vigracket folder in the PNG format, altough we loaded it from the GIF format.

@section{Gaussian Filtering}

In this example, we assume that the blox image has already been loaded as described in the previous section.

We will now apply a ver commonly used low-pass filter onto this image, which uses a gaussian kernel. Since
low-pass filtering has a kind of "smoothing" effect on the visual image interpretation, these filters are also
often classified as smoothing filters.

We apply a filter using a Gaussian with a standard deviation of 1 pixel using:
@racketblock[(define smoothImage (gsmooth myImage 1.0))]

To observe the result of this filter, we use the viewer for visual inspection:
@racketblock[(show-image smoothImage "Blox image gsmoothed with sigma=1.0")]

To observe the influence of different parameter choices, we may use classical functional processing.
E.g. if we are interested in the results for sigma in @code{'(1.0 2.0 3.0 4.0 5.0)},
we define a parameter list and work on that:

@racketblock[
(define parameters '(1.0 2.0 3.0 4.0 5.0))
(define smoothImages
        (map (curry gsmooth myImage) parameters))
(define windowTitles
        (map (curry format "Blox image gsmoothed with sigma=~a") parameters))
(map show-image smoothImages windowTitles)]

This opens 5 image viewers with the correspondings window titles. If you want to store these five images
with meaningful filenames, type:

@racketblock[
(define filenames
        (map (compose (build-path vigracket-path)
                      (curry format "images/blox-gsmoothed-sigma~a.png"))
             parameters))
(map save-image smoothImages filenames)]

@section{Watershed Segmentation}
