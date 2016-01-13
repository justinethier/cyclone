# User Manual for the Cyclone Scheme->C Compiler

- [Introduction](#introduction)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
  - Compiling Scheme files / libraries
    include requirements on input files, generated files, etc
  - Command line options
    - especially include debug ones, such as -t (and what about -d?)
  - Interpreter
- Language Details
  - explain how programs are setup
  - outline scheme language based on r7rs, link to it.
    explain differences between cyclone implementation and r7rs, or again at least link to them
  - provide API, or at least links to the API
  - what else?
- Foreign Function Interface
- Debugging
  include profiling instructions?
- Limitations???
- [Licensing](#licensing)
- [References and Further Reading](#references-and-further-reading)


# Introduction
Cyclone is an experimental Scheme-to-C compiler that uses a variant of the [Cheney on the MTA](http://www.pipeline.com/~hbaker1/CheneyMTA.html) technique to implement full tail recursion, continuations, generational garbage collection, and native threads.

TODO: Contact
  - include information on bug reports (or should that go towards the beginning?)

# Requirements

Tested on Linux, x86 (32-bit) and ARM

Required packages:
- [Concurrency Kit](http://concurrencykit.org/)

    The best way to install libck is via a package manager such as `apt-get`. But if a package is not available for this library it can also be built from source. Just replace `0.5.0` below with the latest version available from their website:
    
        wget http://concurrencykit.org/releases/ck-0.5.0.tar.gz
        tar xfz ck-0.5.0.tar.gz ; cd ck-0.5.0 ; ./configure && make all && sudo make install
        sudo ldconfig
    
- make
- gcc

# Installation
TODO: installation procedure for cyclone-bootstrap
TODO: installation procedure for development????

# Usage

# Licensing
Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).

# References and Further Reading

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- [CHICKEN Scheme](http://www.call-cc.org/)
- [Chibi Scheme](https://github.com/ashinn/chibi-scheme)
- [Compiling Scheme to C with closure conversion](http://matt.might.net/articles/compiling-scheme-to-c/), by Matt Might
- Implementing an on-the-fly garbage collector for Java, by Domani et al
- [Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html), by Christian Queinnec
- Portable, Unobtrusive Garbage Collection for Multiprocessor Systems, by Damien Doligez and Georges Gonthier
- [R<sup>5</sup>RS Scheme Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/)
- [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html), by Harold Abelson and Gerald Jay Sussman
- [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf), by Marc Feeley

