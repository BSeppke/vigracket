#lang scribble/manual

@title{Examples}

In this section, we have collected some very basic examples. More examples and uses of the
included image processing algorithms can be found in the file "examples.rkt", which is part
of the vigracket module.

@section{Hello Image}

Instead of a classical "Hello World", we present a "Hello Image" program. This program demonstrates
the use of the vigracket module to load, show and save images. Before we start, we need to load the
vigracket itself:

@racketcode[(require vigracket)]

Now we can load an arbitrary image, e.g. the Blocks-World image, which is part of the vigracket module:

@racketcode[
 (define myImage (load-image  
			(build-path vigracket-path "images/blox.gif")))]

Afterwards, the symbol @code{myImage} contains the image in the vigracket representation (a list of intensity arrays).
Since the image is greyscale, we will have a list with one element, the array of grey values.
We now may want to take a closer look to the image using the included image viewer:

@racketcode[(show-image myImage „Blox-Bild“)]

This opens the viewer in a new window and shows the image.

If we are interested in a few statistics of the image, we may use the functional extensions. As an example, to
estimate the maximum intensity value of the loaded @code{myImage}, we can use the folding higher order function @code{image-foldl} like:

@racketcode[(define image-max (image-foldl max 0.0 myImage))]

If we want to save the image without any procedures, we can enter:

@racketcode[(save-image myImage 
			(build-path vigracket-path "images/blox.png"))]

This saves the image in the vigracket folder in the PNG forma, altough we loaded it from the GIF format.

@section{Gaussian Filtering}

@section{Watershed Segmentation}
