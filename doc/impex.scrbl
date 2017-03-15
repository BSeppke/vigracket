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
witing hdf5 and OpenEXR datasets.

@centered{
 @bold{Please note that nearly all image formats (except TIFF) are NOT able to store
 floating point values. Instead, the image intensities are scaled from Min...Max to the fixed
 values 0...255 while exporting.

 If you need the actual values of the image, save them in the TIFF format!}}


@defproc[
 (load-image [filename string?])
         image?]{
  Reads an image from the file system. Although relative filenames are allowed, please try to
  use absolute filenames. It the file is not found or could not be read, an error is thrown.
}

@defproc[
 (save-image [image image?][filename string?])
         boolean?]{
  Saves an image to the file system. Please not, that if you are not using the TIFF format, the values
 of the image's pixels may be rescaled. If the image already exists, it will be overwritten without warning.
 Returns #t on success.
}
