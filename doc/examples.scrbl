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

We apply a filter using a Gaussian kernel with a standard deviation of 1 pixel using:
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

This saves the five smoothing results to your hard disk.

@section{Watershed Segmentation}

To conclude the examples, we present the application of a selected segmentation approach and the
use of the regions' features array. Herein, we use an RGB-image, which is shipped with the vigracket
module. It is loaded using:

@racketblock[(define img (loadimage (build-path vigracket-path "images/lenna_face.png")))]

To segment the image into smaller (super pixel) region, we have selected the region-growing watershed transform.
to get meaningful results, this segmentation approach should be carried out on a gradient magnitude
image, rather than on the image itself.

We have selected the gradient magnitude at scale 2.0 of the green band for this demonstration:

@racketblock[(define label_img (watersheds-rg (ggradient (image->green img) 2.0)))]

The resulting label image is of the same size as the image, but contains a label for each pixel, which can be
considered as the region id of that pixel. Using the vigracket's region feature inspection, we can now derive 
some basic statistics for all of the regions:

@racketblock[(define img_stats (extractfeatures img label_img))]

Referring to the documentation of the above method for feature extraction, the values for mean red, green and blue
intensity can be found in the colums 13, 14, and 15 of the @code{img_stats} result. Each row indicates one region id.
Thus, to get the mean intensity images from the label image and the statistics, we may first define a accessor function
for the feature statistics:

@racketblock[
 (define (region->meanColorBand region_id col_id)
         (image-ref img_stats col_id (inexact->exact (round region_id)) 0))]

Using this access, we can define the mean color image using standard higher order mapping procedures:

@racketblock[
 (define meancolor_img
   (list (band-map  (curryr region->meanColorBand 13) (first segmentation))
         (band-map  (curryr region->meanColorBand 14) (first segmentation))
         (band-map  (curryr region->meanColorBand 15) (first segmentation))))]

You may now want to have a look on the image, e.g. with crack edges beween the regions in black color:

@racketblock[(show-image (regionimagetocrackedgeimage meancolor_img 0.0)
                         "img - watershed regions (mean color)")]

And you may also save the resulting image or do anything else with it. Please note, that this procedure only works
for RGB images and a single label image (segmentation). Else, the indizes are not 13, 14, and 15 but stored band-wise.