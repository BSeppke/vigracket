#lang scribble/manual

@title[#:tag "Installation"]{Installation}

The installation of the vigracket module should be quite straightforward. Moreover, the module
is tested and known to work under Linux, Windows and macOS. However, under some operating
systems, more prerequisites are required. 

@section{Prerequisites}

Before installing the vigracket module under any operating system other than Windows, please
make sure, that you have the vigra computer vision library (version >= 1.11.0) as well as the FFTW and
FFTW-single library already installed on your machine.

Unter macOS one of the easiest way to achieve this, is to download the MacPorts package manager
from @url{http://macports.org}. After download and installation of MacPorts, you are able to install
vigra by typing:

@code{sudo port install vigra}

For Linux-based operating systems you can either use a package management system of your choice
if it provides development package for vigra version >= 1.11.0. If this is not the case, just grab the 
current version of the vigra library from: @url{http://ukoethe.github.io/vigra/} and install it as described
there.

For windows, the vigra binaries come with the complete vigracket releases. But, since these binaries
have been compiled with Visual Studio 2015, you may need to install the Visual C++ 2015 runtime either in 32-
or 64-bits, depending on your Racket version. Installing both is probably the easiest way to make sure they are
available. You can acquire these runtimes directly from Microsoft:

@url{https://www.microsoft.com/en-us/download/details.aspx?id=48145}


@section{Getting and Installing}

The easiest way of getting the vigracket module is to grab a complete
release (zip-file) from @url{https://github.com/bseppke/vigracket/releases}. However, if you prefer cloning and
like to fork from my GitHub page, please make sure to clone the included submodule vigra_c as well. Without this
submodule, you will not be able to install the vigracket module.

The installation itself is as easy as opening the file "install.rkt" in DrRacket and pressing the "Run"-button.
This will copy the module to your local collects' path and build the bindings under macOS and Linux.

@section{Testing}

After a successful installation you may use the vigracket module by including it into your own projects by
typing:

@racketblock[(require vigracket)]

To get an impression of the functionality, you may also open the shipped examples by opening the file "examples.rkt"
in DrRacket and running it. 