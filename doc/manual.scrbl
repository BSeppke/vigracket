#lang scribble/manual
@;{
   THIS IS THE MAIN DOCUMENTATION FILE FOR VIGRACKET

   To build it using DrRacket, simply press the "Scribble PDF" or "Scribble HTML"
   buttons in the toolbar.
   
   However, I strongly recommend to build this manual as a multi-page HTML file
   by using the commandline tool, which creates a new folder "manual" with all
   the manuals files included:

   > RACKET_BIN_PATH/scribble --htmls manual.scrbl
}

@(require (for-label racket))

@title{vigracket}
@author{Benjamin Seppke}

Use the power of the computer vision library VIGRA by means of the functional programming language Racket (former: Scheme). The interaction between both (c++ and functional) worlds is realized by using Racket's Foreign Function Interface (FFI) and the vigra_c wrapper library.
 
@include-section["introduction.scrbl"]
@include-section["installation.scrbl"]
@include-section["datastructures.scrbl"]
@include-section["impex.scrbl"]
@include-section["imageprocessing.scrbl"]
@include-section["visualization.scrbl"]
@include-section["examples.scrbl"]
