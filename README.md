# vigracket

Use the power of the computer vision library VIGRA by means of the functional programming language Racket (former: Scheme). The interaction between both (c++ and functional) worlds is realized by using Racket's Foreign Function Interface (FFI) and the vigra_c wrapper library.

This is the n-th release of the vigra-Racket bindings!


### 1. Prerequisites
For Linux and Mac OS X, the vigra Computer Vision library needs to be installed at version >=1.11.0. There are different ways to achieve this, but here are my recommendations:

##### For Linux:
Use your package manager, to install the libvigra-dev package. If this package is of an old version <1.11.0, then download the current vigra master branch and build and over-install the new version using:

```bash
git clone https://github.com/ukoethe/vigra 
cd vigra 
mkdir build
cd build
cmake ..
make
sudo make install
```

##### For Mac OS X
I prefer the MacPorts package system. You may download it from https://macports.org. 
After the installation, you can install all required prerequisites using:

```bash
sudo port install cmake
sudo port install vigra
```

##### For Windows
You need to have installed the MS VC-Runtime (2015) in order to get the shipped binaries running. 
The runtime can be downloaded from: https://www.microsoft.com/en-us/download/details.aspx?id=48145
 
### 2. Installation
The installation of the vigracket-bindings is quite easy. Just unzip the complete-release package, if you have downloaded a complete-release package. I strongly recommend using this package for productive work.

If you want to use the git master branch, you will need to sync the included vigra_c submodule by typing:

```bash
git clone https://github.com/bseppke/vigracket
cd vigracket
git submodule init
git submodule update --remote
```
Inside the vigracket-directory you will find a file called "install.rkt". Open this file in DrRacket and execute it once. This will copy the vigracket files to the local collects directory. Alternatively, you may also perform this on your shell, if the Racket-installation is in your ```PATH``` environment variable using: 

```bash
racket install.rkt
```


### 3. Using the vigracket module
After successful installation, you can include the package by calling


```racket
(require vigracket)
```

On the first call and on any failure of loading the vigra_c lib, this will start the auto-build of the vigra_c bindings. This should build the vigra_c-wrapper library under Linux and Mac OS X or copy the correct binaries for Windows.
You should be able to run the examples provided by the vigracket library by means of loading the "examples.rkt" file into DrRacket and pressing the "Run" button.

### 4. Documentation 
The documentation can be found inside the doc/manual folder for the complete-release package. You can also generate the docs from the scribble files using the script located in the docs folder. If the Racket-installation is in your ```PATH``` environment variable, you will be able to generate the docs by:

```bash
cd docs 
scribble --htmls manual.scrbl
```

The  newly created html files will be stored in the manual subfolder. Note, that this cannot be performed inside DrRacket, since DrRacket is currently missing the setting for multi-page docs generation. 
