vigracket
=========

Use the power of the computer vision library VIGRA by means of the functional programming language Racket (former: Scheme). The interaction between both (c++ and functional) worlds is realized by using Racket's Foreign Function Interface (FFI) and the vigra_c wrapper library.
This is the first release of the vigra-Racket bindings!


1. Prerequisites
-----------------------------------

For Linux and Mac OS X, the vigra Computer Vision library needs to be installed. I recommend the use of a version > 1.9.0, but lower versions may also work. The easiest way to do so, is using your favorite package manager under linux or using MacPorts und  Mac OS X. Otherwise you need to pay attention to install all the needed dependencies on your own.

<b>Attention:</b> Under linux (Ubuntu) I encountered an installation problem of the vigra, such that `vigra-config --libs` pointed to a non-existing file. I was able to solve this by copying the necessary binary to the right position:
> sudo cp /usr/local/lib/libvigraimpex.* /usr/lib/x86_64-linux-gnu

Note, that for Windows, you also need to have installed the MS VC-Runtime (2010) in order to get these binaries running.
 
2. Installation
-----------------------------------

The istallation of the vigracket-bindings are quite easy. Just copy (or link) your release/repository to the collects path of Racket, e.g.:

> /Users/USERNAME/Library/Racket/6.0/collects

under Mac OS X or:

> C:\Users\USERNAME\AppData\Roaming\Racket\6.0\collects

for Windows. <b>Please do not use the system collects dir!</b> Permission rights may hinder a correct working behaviour here!

3. Auto-build of the c-wrapper
-----------------------------------

After copying or linking to the target destination, you should be able to run the examples provided by the vigracket library by means of loading the "examples.rkt" file into DrRacket and pressing the "Run" button.
This should build the vigra_c-wrapper library under Linux and Mac OS X on the first call and should copy the correct binaries for Windows.

If this does not run out-of the box, you may need to change the systems path-variable in order to find the vigra library. However, if the vigra installation is in the standard system path, you can simply replace line 28 in file "config.rkt" with

> (define (system-env arg) (system arg))

and then start to run the examples again. If this doesn't work either, look for a built binary file at "vigracket/vigra_c/bin/libvigra_c.so". Under some circumstances, the right may not suffice for Racket to copy it to the vigracket directory. In case, please do that on your own.

