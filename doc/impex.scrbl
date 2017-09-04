#lang scribble/manual

@title{Import and Export of Images}
@defmodule[vigracket/impex]


In general, the VIGRA is able to read and write from/to various image formats. Depending on the
image format, it is possible to import/export:

@itemize{
    @item{Grey value images (1 band)}
    @item{RGB value images (3 bands)}
    @item{RGBA value images (4 bands)}
}

The number of available formats might be restricted during compile time
of the vigraimpex module. E.g. the shipped windows binaries provide support for the
import and export of the following image types, which are dermined by their filetypes:

@itemize{
    @item{Windows Bitmap (.bmp)}
    @item{Compuserve GIF (.gif)}
    @item{JPEG (.jpg, .jpeg)}
    @item{PNG (.png)}
    @item{PNM (.pnm)}
    @item{Sun Raster format (.sun, .ras)}
    @item{TIFF (.tiff, .tif)}
    @item{Khoros VIFF (.viff, .vif)}
}

Your self-compiled version of the vigraimpex library may als be capable of reading and
witing OpenEXR datasets. HDF5 is currently not supported by vigracket.

@centered{
 @bold{Please note that nearly all image formats (except TIFF) are NOT able to store
 floating point values.

  Thus, if you need the actual floating point values of the image, save them in the TIFF format!

  Refer to the documentation of the @code{exportImage} function below for more information.}}


@defproc[
 (load-image [filename string?])
         image?]{
  Reads an image from the file system. Although relative filenames are allowed, please try to
  use absolute filenames. It the file is not found or could not be read, an error is thrown.
}

@defproc[
 (save-image [img image?][filename string?] [rescale_range boolean? #t])
         boolean?]{
 Saves an image to the file system.

 Please note, that if you are not using the TIFF format, the values
 of the image's pixels will change. However, you can influece the change by means of the parameter @code{rescale_range}.
 If set to true (default), the intensity values will be rescaled from Min...Max to 0..255. If set to false, the values will
 be clipped at 0 and 255 respectively.

 If the image already exists, it will be overwritten without warning.

 Returns #t on success.
}
