#lang scribble/manual
 
@title[#:tag "introduction"]{Introduction}

VIGRA stands for "Vision with Generic Algorithms". It's a novel computer vision library that puts its main emphasize on customizable algorithms and data structures. By using template techniques similar to those in the C++ Standard Template Library, you can easily adapt any VIGRA component to the needs of your application, without thereby giving up execution speed. VIGRA was originally designed and implemented by Ullrich Köthe. To get more information, visit the VIGRA Homepage!
Motivaton of the interfaces to other programming languages

As already mentioned, the VIGRA is a pure, template driven C++ library. This may be the language of choice in many cases, e.g. if you need very fast algorithms that can be written using the concrete object-oriented paradigma and the wonderful templates c++ offers.

On the other hand, there are enough reasons not to use C++, e.g. that it is not an interactive language because of the necessary code&compile-cycles and it is very hard to use C++ when you want to use another programming paradigma as the functional paradigma (of e.g. Lisp & Scheme) Additionally, C++ is still quite a low-level language and therefore may not be the language you want to use for high-level task where LISP would be preferable.

As a summary, we can say that just the first point alone (the missing interactivity) is strong enough to motivate the use of the image processing algorithms that VIGRA offers. For the same reason, Python became that popular, and there is also some work going on a specialized python interface beside the interfaces described here
General structure of the libraries

The easiest way to intergrate C/C++ code written in other programming languages is a generic C-interface to a C-dynamic library at runtime. That is why we also use this interface for all libraries presented herein. This interface allows us to load C-conform "shared libraries" (for Windows: DLLs, for Mac OS X: dylibs and under Linux: so) at any time interactively and to call the included functions directly.
Unfortunately, the VIGRA only offers a dynamic library for the import- / export-facilities of pictures. Because of this. the first step is to abstract from the C++ image processing functions of the VIGRA to a C-dynamic-library-wrapper (VIGRA_C) containing everything we want to call from the other languages. This dynamic library is the "glue component" between the VIGRA on the on side and the other languages on the other side, that will hide the low level C-calls and translate the functions into readable names.
 