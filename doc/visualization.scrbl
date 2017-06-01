#lang scribble/manual

@title{Image Visualization}

To inspect the images and to qualitatively explore and compare the results of image
processing algorithms, the vigracket module offers a variety of visualization approaches.
It is either possible to open an image viewer window, which lets you inspect the intensity
values of a grey value or RGB(A) image, or to convert vigracket images into Racket's
native bitmap format.

@section{The vigracket Image Viewer}

The easiest and probably most powerful way to inspect images is due to the included image
viewer. This viewer is based on the Racket GUI package and needs no further libraries or
packages.

@defmodule[vigracket/viewer]

@defproc[
 (show-image [img image?] [window_title string? "vigracket Image Viewer"])
 void]{
  Displays a given image in a new window. This viewer only works for 1-band (grey value) images
  and for 3/4-band RGB(A) images. Please note, that this viewer does not rescale the pixel values
  but casts them to the range of bytes (0...255). This may result in overflow artifacts on low visibility
  of small intensities.

  Each viewer can be assigned a window title to make the evaluation easier for similar resulting images.

  Since the viewer uses Racket's GUI elements scroll area and a drawing canvas, the following interactions
  have been implemented:
  
  @itemize{
    @item{@bold{Mouseover}

         The current mouse posistion as well as the intensity values at that position are shown in the status bar}
    @item{@bold{Mouse wheel}

         The mouse wheel lets you zoom in or out the current image view. If you do not have any mouse wheel,
         you may use Ctrl-"+" to zoom in, Ctrl-"-" to zoom out and Ctrl-"0" to reset the zoom to original (100%) level.
         Zooming an image does involve nearest neighbor interpolation, thus you will see the single pixels here.}
    @item{@bold{Scrollbars}

          Whenever the image plane is larger than the window, scrollbars will appear. You may want to use these bars
          to navigate inside the image.}
}

}


@section{Conversion from/to Racket Images}

Another way of viewing images as well as integrating images into other Racket programs is the conversion of vigracket's
image format into Racket's bitmap format.

@defmodule[vigracket/convert]

@defproc[
 (image->bitmap [img image?])
 bitmap%]{
  Convert a given (vigracket) image to a Racket RGBA-bitmap. This only works for 1-band (grey value) images
  and for 3/4-band RGB(A) images. Please note, that this conversion does not rescale the pixel values
  but casts them to the range of bytes (0...255). This may result in overflow artifacts on low visibility
  of small intensities.
}


The resulting bitmaps may then be used in GUIs or may be overlaid with drawings, e.g. by means of the @code{2htdp/image} module
However, if you want to want to use both modules @code{vigracket} and @code{2htdp/image} simultaneously, you have to avoid name
clashes using:

@racketblock[
(require vigracket)
(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width plt-image-width)
                    (image-height plt-image-height)))
]

@defproc[
 (bitmap->image [bitmap bitmap%])
 image?]{
  Convert a given Racket RGBA-bitmap to a (vigracket) image. If @code{(send bitmap is-color?)} is true, it will either create
  a vigracket RGB-image or if @code{(send bitmap has-alpha-channel?)} is true, and RGBA image. Else, it will create a grey value image
  from the bitmap.
}
