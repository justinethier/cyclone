![Cyclone Scheme](docs/images/cyclone-logo-04-header.png "Cyclone Scheme")

[![Travis CI](https://travis-ci.org/justinethier/cyclone.svg?branch=master)](https://travis-ci.org/justinethier/cyclone)

[![Github CI - Linux](https://github.com/justinethier/cyclone-bootstrap/workflows/Ubuntu%20Linux%20Build/badge.svg)](https://github.com/justinethier/cyclone-bootstrap)

[![Github CI - MacOS](https://github.com/justinethier/cyclone-bootstrap/workflows/MacOS%20Build/badge.svg)](https://github.com/justinethier/cyclone-bootstrap)

[![Github CI - MacOS Homebrew](https://github.com/justinethier/cyclone-bootstrap/workflows/MacOS%20Homebrew%20Build/badge.svg)](https://github.com/justinethier/cyclone-bootstrap)

[![Github CI - Windows](https://github.com/justinethier/cyclone-bootstrap/workflows/Windows%20MSYS2%20Build/badge.svg)](https://github.com/justinethier/cyclone-bootstrap)

Cyclone Scheme is a brand-new compiler that allows real-world application development using the R<sup>7</sup>RS Scheme Language standard. We provide modern features and a stable system capable of generating fast native binaries.

[Cheney on the MTA](https://github.com/justinethier/cyclone/raw/master/docs/research-papers/CheneyMTA.pdf) is used by Cyclone's runtime to implement full tail recursion, continuations, and generational garbage collection. In addition, the Cheney on the MTA concept has been extended to allow execution of multiple native threads. An on-the-fly garbage collector is used to manage the second-generation heap and perform major collections without "stopping the world".

# Features

- Support for the majority of the Scheme language as specified by the latest [R<sup>7</sup>RS standard](docs/Scheme-Language-Compliance.md). 
- New features from R<sup>7</sup>RS including libraries, exceptions, and record types.
- Built-in support for Unicode strings and characters.
- Hygienic macros based on `syntax-rules`
- Low-level explicit renaming macros
- Guaranteed tail call optimizations
- Native multithreading support
- A foreign function interface that allows easy integration with C
- A concurrent, generational garbage collector based on Cheney on the MTA
- Includes an optimizing Scheme-to-C compiler,
- ... as well as an interpreter for debugging
- A [Package Manager](https://github.com/cyclone-scheme/cyclone-winds) and a growing list of packages.
- Support for [many popular SRFI's](docs/API.md#srfi-libraries)
- Online user manual and API documentation
- Support for Linux, Windows, FreeBSD, and Mac platforms. 
- Known to run on x86-64, x86, and Arm (Raspberry Pi) architectures.

# Installation

There are several options available for installing Cyclone:

## Docker 
![Docker](docs/images/docker-thumb.png "Docker")

Cyclone can be run from a [Docker Image](https://hub.docker.com/r/cyclonescm/cyclone):
    
    docker run -it cyclonescm/cyclone bash
    
## Homebrew 
![Homebrew](docs/images/homebrew-thumb.png "Homebrew")

Mac (and Linux!) users wanting to use Homebrew can do the following.

Note if Homebrew is not already installed: follow the instructions at [https://brew.sh/](https://brew.sh/) to install the homebrew package manager. 

    brew tap cyclone-scheme/cyclone
    brew install cyclone-scheme/cyclone/cyclone-bootstrap

## Arch Linux 
![Arch Linux](docs/images/arch-linux-thumb.png "Arch Linux")

Arch Linux users can install using the [AUR](https://aur.archlinux.org/packages/cyclone-scheme/):

    git clone https://aur.archlinux.org/cyclone-scheme.git
    cd cyclone-scheme
    makepkg -si

## Build from Source
![Build from Source](docs/images/build-thumb.png "Build from Source")

To install Cyclone on your machine for the first time on Linux, Windows, FreeBSD, and for Mac users wanting to install without using Homebrew, use [**cyclone-bootstrap**](https://github.com/justinethier/cyclone-bootstrap) to build a set of binaries. Instructions are provided for Linux, Mac, Windows (via MSYS), and FreeBSD 12.

# Getting Started

After installing you can run the `cyclone` command to compile a single Scheme file:

    $ cyclone examples/fac.scm
    $ examples/fac
    3628800
    
And the `icyc` command to start an interactive interpreter. Note you can use [`rlwrap`](http://linux.die.net/man/1/rlwrap) to make the interpreter more friendly, EG: `rlwrap icyc`:

    $ icyc
    
                  :@
                @@@
              @@@@:
            `@@@@@+
           .@@@+@@@      
           @@     @@     Cyclone Scheme->C compiler
          ,@             http://justinethier.github.io/cyclone/
          '@
          .@
           @@     #@     (c) 2014-2019 Justin Ethier
           `@@@#@@@.     Version 0.11
            #@@@@@
            +@@@+
            @@#
          `@.
       
    cyclone> (write 'hello-world)
    hello-world

Read the documentation below for more information on how to use Cyclone.

# Package Manager

![Cyclone Winds](docs/images/cyclone-winds-small.png "Cyclone Winds")

The `cyclone-winds` package manager provides the ability to install packaged libraries and programs for Cyclone. See the [cyclone-winds](https://github.com/cyclone-scheme/cyclone-winds#cyclone-winds) site for more information.

The [Cyclone-Winds wiki](https://github.com/cyclone-scheme/cyclone-winds/wiki) contains a full list of packages with documentation.

# Documentation

- The [User Manual](docs/User-Manual.md) covers in detail how to use Cyclone and provides information on the Scheme language features implemented by Cyclone.

- An [API Reference](docs/API.md) is available for all libraries provided by Cyclone, including a complete alphabetical listing.

- If you need a resource to start learning the Scheme language you may want to try a classic textbook such as [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html).

- Finally, this [benchmarks](http://ecraven.github.io/r7rs-benchmarks/benchmark.html) page by [ecraven](https://github.com/ecraven) compares the performance of Cyclone with other Schemes.

# Example Programs

Cyclone provides several example programs, including:

- [Tail Call Optimization](examples/tail-call-optimization.scm) - A simple example of Scheme tail call optimization; this program runs forever, calling into two mutually recursive functions.

- [Threading](examples/threading) - Various examples of multi-threaded programs.

- [Game of Life](examples/game-of-life) - The [Conway's game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) example program and libraries from R<sup>7</sup>RS.

- [Game of Life PNG Image Generator](examples/game-of-life-png) - A modified version of game of life that uses libpng to create an image of each iteration instead of writing it to console. This example also demonstrates basic usage of the C Foreign Function Interface (FFI).

- Finally, the largest program is the compiler itself. Most of the code is contained in a series of libraries which are used by [`cyclone.scm`](cyclone.scm) and [`icyc.scm`](icyc.scm) to create executables for Cyclone's compiler and interpreter.

# Compiler Internals

- [Writing the Cyclone Scheme Compiler](docs/Writing-the-Cyclone-Scheme-Compiler-Revised-2017.md) provides high-level details on how the compiler was written and how it works.

- There is a [Development Guide](docs/Development.md) with instructions for common tasks when hacking on the compiler itself.

- Cyclone's [Garbage Collector](docs/Garbage-Collector.md) is documented at a high-level. This document includes details on extending Cheney on the MTA to support multiple stacks and fusing that approach with a tri-color marking collector.

- The garbage collector was subsequently enhanced to support [Lazy Sweeping](https://github.com/justinethier/cyclone/blob/master/docs/Garbage-Collection-Using-Lazy-Sweeping.md) which improves performance for a wide range of applications.

# License

Copyright (C) 2014 [Justin Ethier](http://github.com/justinethier).

Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).
